#
# Copyright (c) 2008 rPath, Inc.
#
# This program is distributed under the terms of the Common Public License,
# version 1.0. A copy of this license should have been distributed with this
# source file in a file called LICENSE. If it is not present, the license
# is always available at http://www.rpath.com/permanent/licenses/CPL-1.0.
#
# This program is distributed in the hope that it will be useful, but
# without any warranty; without even the implied warranty of merchantability
# or fitness for a particular purpose. See the Common Public License for
# full details.
#

import StringIO
import os
import subprocess
from xml import sax

from lxml import etree

def prettyPrint(func):
    def wrapper(*args, **kwargs):
        res = func(*args, **kwargs)
        # XXX: It is fairly inefficient to serialize an
        # ElementTree-like object to XML, then parse it back into an
        # lxml.etree.ElementTree object, then serialize it again just to
        # pretty-print it. Maybe we whould just have _toXml write an
        # ElementTree object to begin with?
        tree = etree.parse(StringIO.StringIO(res))
        res = etree.tostring(tree, pretty_print = True,
            xml_declaration = True, encoding = 'UTF-8')
    return wrapper

class _AbstractNode(object):
    __slots__ = ['_children', '_nsMap', '_name', '_nsAttributes',
                 '_otherAttributes', ]
    def __init__(self, attributes = None, nsMap = None, name = None):
        self._children = []
        self._nsMap = nsMap or {}
        self._setAttributes(attributes)
        if name is not None:
            self.setName(name)

    def setName(self, name):
        nsName, tagName = splitNamespace(name)
        if nsName is not None and nsName not in self._nsMap:
            raise Exception("Reference to undefined namespace %s" % nsName)
        self._name = (nsName, tagName)

    def getName(self):
        return unsplitNamespace(self._name[1], self._name[0])

    def addChild(self, childNode):
        # If the previous node in the list is character data, drop it, since
        # we don't support mixed content
        if self._children and isinstance(self._children[-1], unicode):
            self._children[-1] = childNode.finalize()
        else:
            if childNode.getName() in getattr(self, '_singleChildren', []):
                setattr(self, childNode.getName(), childNode.finalize())
            else:
                self._children.append(childNode.finalize())

    def iterChildren(self):
        if hasattr(self, '_childOrder'):
            return orderItems(self._children, self._childOrder)
        return iter(self._children)

    def finalize(self):
        return self

    def characters(self, ch):
        if self._children:
            if isinstance(self._children[-1], unicode):
                self._children[-1] += ch
            # We don't support mixed contents, so don't bother adding
            # characters after children
        else:
            self._children.append(ch)

    def getNamespaceMap(self):
        return self._nsMap.copy()

    def iterAttributes(self):
        for nsName, nsVal in sorted(self._nsAttributes.items()):
            if nsName is None:
                yield ('xmlns', nsVal)
            else:
                yield ('xmlns:%s' % nsName, nsVal)
        for (nsName, attrName), attrVal in self._otherAttributes.items():
            if nsName is None:
                yield (attrName, attrVal)
            else:
                yield ("%s:%s" % (nsName, attrName), attrVal)

    def getAttribute(self, name, namespace = None):
        return self._otherAttributes.get((namespace, name))

    def getChildren(self, name, namespace = None):
        tagName = unsplitNamespace(name, namespace)
        return [ x for x in self.iterChildren()
            if hasattr(x, 'getName') and x.getName() == tagName ]

    def getText(self):
        text = [ x for x in self._children if isinstance(x, (str, unicode)) ]
        if not text:
            return ''
        return text[0]

    def getElementTree(self, parent = None):
        if self._name[0] is None:
            name = self._name[1]
        else:
            name = "{%s}%s" % (self._nsMap[self._name[0]], self._name[1])

        attrs = {}
        for (nsName, attrName), attrVal in self._otherAttributes.items():
            attrName = self._buildElementTreeName(attrName, nsName)
            attrs[attrName] = attrVal

        if parent is None:
            elem = etree.Element(name, attrs, self._nsAttributes)
        else:
            elem = etree.SubElement(parent, name, attrs, self._nsAttributes)
        for child in self.iterChildren():
            if hasattr(child, 'getElementTree'):
                child.getElementTree(parent = elem)
            elif isinstance(child, unicode):
                elem.text = child
            elif isinstance(child, tuple):
                self._getElementTreeFromTuple(child, elem)
            else:
                # XXX
                pass
        return elem

    def _getElementTreeFromTuple(self, child, parent = None):
        nsName, tagName = splitNamespace(child[0])
        elemName = self._buildElementTreeName(tagName, nsName)
        if parent is None:
            nelem = etree.Element(elemName)
        else:
            nelem = etree.SubElement(parent, elemName)
        val = None
        if isinstance(child[1], bool):
            val = child[1] and 'true' or 'false'
        elif isinstance(child[1], (str, unicode)):
            val = child[1]
        else:
            val = str(child[1])
        nelem.text = val

    def _setAttributes(self, attributes):
        self._nsAttributes = {}
        self._otherAttributes = {}
        if attributes is None:
            return
        nonNsAttr = []
        for attrName, attrVal in attributes.items():
            arr = attrName.split(':', 1)
            if arr[0] != 'xmlns':
                # Copy the tag aside, we may need to qualify it later
                if len(arr) == 1:
                    # No name space specified, use default
                    attrKey = (None, attrName)
                else:
                    attrKey = tuple(arr)
                nonNsAttr.append((attrKey, attrVal))
                continue
            if len(arr) == 1:
                nsName = None
            else:
                nsName = arr[1]
            self._nsMap[nsName] = attrVal
            self._nsAttributes[nsName] = attrVal
        # Now walk all attributes and qualify them with the namespace if
        # necessary
        for (nsName, attrName), attrVal in nonNsAttr:
            if nsName is not None and nsName not in self._nsMap:
                raise Exception("Reference to undefined namespace %s" % nsName)
            self._otherAttributes[(nsName, attrName)] = attrVal

    def _buildElementTreeName(self, name, namespace = None):
        if namespace is None:
            return name
        return "{%s}%s" % (self._nsMap[namespace], name)


class BaseNode(_AbstractNode):
    pass

class GenericNode(BaseNode):
    """
    Base node for all data classes used by SAX handler. Neither this class,
    nor any descendent needs to be instantiated. They should be registered
    with instances of the DataBinder class.

    This class serves as the base datatype. This is the default node type
    if nothing else is specified, thus it's not useful to register this
    class.

    By default, _addChild will add the childNode to a list. specifying an
    attribute in _singleChildren will cause the value to be stored directly.
    """

    def __init__(self, attributes = None, nsMap = None):
        BaseNode.__init__(self, attributes = attributes, nsMap = nsMap)

class IntegerNode(BaseNode):
    """
    Integer data class for SAX parser.

    Registering a tag with this class will render the text contents into
    an integer when finalize is called. All attributes and tags will be lost.
    If no text is set, this object will default to 0.
    """
    def finalize(self):
        text = self.getText()
        try:
            return int(text)
        except ValueError:
            return 0

class StringNode(BaseNode):
    """
    String data class for SAX parser.

    Registering a tag with this class will render the text contents into
    a string when finalize is called. All attributes and tags will be lost.
    If no text is set, this object will default to ''.
    """
    def finalize(self):
        text = self.getText()
        return text

class NullNode(BaseNode):
    """
    Null data class for SAX parser.

    Registering a tag with this class will render the text contents into
    None when finalize is called. All attributes and tags will be lost.
    All text will be lost.
    """
    def finalize(self):
        pass

class BooleanNode(BaseNode):
    """
    Boolean data class for SAX parser.

    Registering a tag with this class will render the text contents into
    a bool when finalize is called. All attributes and tags will be lost.
    '1' or 'true' (case insensitive) will result in True.
    """
    def finalize(self):
        text = self.getText()
        return text.strip().upper() in ('TRUE', '1')

class DictNode(BaseNode):
    """
    Dict container class for SAX parser.

    Registering a tag with this class will return the __dict__  of this class
    when finalize is called. This effectively means all tags will be
    preserved, but all attributes will be lost.
    """
    def finalize(self):
        return dict(x for x in self.__dict__.iteritems() \
                if not x[0].startswith('_'))

class BindingHandler(sax.ContentHandler):
    """
    Sax Content handler class.

    This class doesn't need to be instantiated directly. It will be invoked
    on an as-needed basis by DataBinder. This class interfaces with the
    Python builtin SAX parser and creates dynamic python objects based on
    registered node classes. If no nodes are registered, a python object
    structure that looks somewhat like a DOM tree will result.
    """
    def __init__(self, typeDict = None):
        if not typeDict:
            typeDict = {}
        self.typeDict = typeDict
        self.stack = []
        self.rootNode = None
        sax.ContentHandler.__init__(self)

    def registerType(self, typeClass, name = None, namespace = None):
        if name is None:
            name = typeClass.name
        if namespace is None:
            namespace = getattr(typeClass, 'namespace', None)

        self.typeDict[(namespace, name)] = typeClass

    def startElement(self, name, attrs):
        classType = GenericNode
        nameSpace, tagName = splitNamespace(name)
        if (nameSpace, tagName) in self.typeDict:
            classType = self.typeDict[(nameSpace, tagName)]

        if self.stack:
            nsMap = self.stack[-1].getNamespaceMap()
        else:
            nsMap = {}
        newNode = classType(attrs, nsMap = nsMap)
        newNode.setName(name)
        self.stack.append(newNode)

    def endElement(self, name):
        elem = self.stack.pop()
        if not self.stack:
            self.rootNode = elem.finalize()
        else:
            self.stack[-1].addChild(elem)

    def characters(self, ch):
        elem = self.stack[-1]
        elem.characters(ch)


class DataBinder(object):
    """
    DataBinder class.

    This class wraps all XML parsing logic in this module. As a rough rule
    of thumb, attributes of an XML tag will be treated as class level
    attributes, while subtags will populate an object's main dictionary.

    parseFile: takes a a path and returns a python object.
    parseString: takes a string containing XML data and returns a python
        object.
    registerType: register a tag with a class defining how to treat XML content.
    toXml: takes an object and renders it into an XML representation.

    EXAMPLE:
    class ComplexType(BaseNode):
        _singleChildren = ['foo', 'bar']

    binder = DataBinder()
    binder.registerType('foo', BooleanNode)
    binder.registerType('bar', NullNode)
    binder.registerType('baz', ComplexType)

    obj = binder.parseString('<baz><foo>TRUE</foo><bar>test</bar></baz>')

    obj.foo == True
    obj.bar == 'test'

    EXAMPLE:
    binder = DataBinder()
    class baz(object):
        pass
    obj = baz()
    obj.foo = True
    obj.bar = 'test'
    binder.toXml(obj) == '<baz><foo>true</foo><bar>test</bar></baz>'
    """
    def __init__(self, typeDict = None):
        self.contentHandler = BindingHandler(typeDict)

    def registerType(self, klass, name = None, namespace = None):
        return self.contentHandler.registerType(klass, name = name,
                                                namespace = namespace)

    def parseString(self, data):
        stream = StringIO.StringIO(data)
        return self.parseFile(stream)

    def parseFile(self, stream):
        if isinstance(stream, str):
            stream = file(stream)
        self.contentHandler.rootNode = None
        parser = sax.make_parser()
        parser.setContentHandler(self.contentHandler)
        parser.parse(stream)
        rootNode = self.contentHandler.rootNode
        self.contentHandler.rootNode = None
        return rootNode

    @staticmethod
    def _etree_add(elem, obj):
        if isinstance(obj, list):
            elem.extend(obj)
        else:
            elem.append(obj)

    def _to_etree_r(self, obj, suggestedName = None):
        name = suggestedName or obj.__class__.__name__

        elem = etree.Element(name)
        if isinstance(obj, bool):
            elem.text = (obj and 'true' or 'false')
        elif isinstance(obj, (int, float, long)):
            elem.text = str(obj)
        elif isinstance(obj, (str, unicode)):
            elem.text = obj
        elif isinstance(obj, (list, set, tuple)):
            elem = [ self._to_etree_r(child, suggestedName) for child in obj]
        elif isinstance(obj, dict):
            for childName, child in sorted(obj.iteritems()):
                self._etree_add(elem, self._to_etree_r(child, suggestedName = childName))
        else:
            elem = obj.getElementTree()
        return elem

    @staticmethod
    def _toString(obj):
        if isinstance(obj, (str, unicode)):
            return obj
        return str(obj)

    def toXml(self, obj, suggestedName = None):
        tree = self._to_etree_r(obj, suggestedName)
        res = etree.tostring(tree, pretty_print = True,
            xml_declaration = True, encoding = 'UTF-8')
        return res

    def _toXml(self, obj, suggestedName = None):
        encoding = None
        tree = self._to_etree_r(obj, suggestedName)
        if isinstance(tree, list):
            res = [ etree.tostring(x, pretty_print = False,
                xml_declaration = False, encoding = encoding) for x in tree ]
            res = ''.join(res)
        else:
            res = etree.tostring(tree, pretty_print = False,
                xml_declaration = False, encoding = encoding)
        return res

def splitNamespace(tag):
    """Splits the namespace out of the tag.
    @param tag: tag
    @type tag: C{str}
    @return: A tuple with the namespace (set to None if not present) and
    the tag name.
    @rtype: C{tuple} (namespace, tagName)
    """
    arr = tag.split(':', 1)
    if len(arr) == 1:
        return None, tag
    return arr[0], arr[1]

def unsplitNamespace(name, namespace = None):
    if namespace is None:
        return name
    return "%s:%s" % (namespace, name)

def orderItems(items, order):
    # sort key is a three part tuple. each element maps to these rules:
    # element one reflects if we know how to order the element.
    # element two reflects the element's position in the ordering.
    # element three sorts everything else by simply providing the original
    # item (aka. default ordering of sort)
    return sorted(items,
        key = lambda x: (x.getName() not in order,
                         x.getName() in order and order.index(x.getName()),
                         x.getName()))
