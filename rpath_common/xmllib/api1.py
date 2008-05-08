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
"""
The rPath XML Library API.

All interfaces in this modules that do not start with a C{_}
character are public interfaces.
"""

import StringIO
from xml import sax

from lxml import etree

#{ Exception classes
class XmlLibError(Exception):
    "Top-level exception class"

class UndefinedNamespaceError(XmlLibError):
    "Raised when a reference to an undefined namespace is found"
#}

#{ Base classes
class SerializableObject(object):
    """
    Base class for an XML-serializable object
    """

    # pylint: disable-msg=R0903
    # Too few public methods (1/2): this is an interface
    def getElementTree(self, parent = None):
        """Any serializable object should implement the C{getElementTree}
        method, which returns a hierarchy of objects that represent the
        structure of the XML document.

        @param parent: An optional parent object.
        @type parent: C{SerializableObject} instance
        """
        name = self._getName()

        attrs = {}
        for attrName, attrVal in self._iterAttributes():
            if isinstance(attrVal, bool):
                attrVal = BooleanNode.toString(attrVal)
            elif not isinstance(attrVal, (str, unicode)):
                attrVal = str(attrVal)
            attrs[attrName] = attrVal

        localNamespaces = self._getLocalNamespaces()

        elem = createElementTree(name, attrs, localNamespaces, parent = parent)
        for child in self._iterChildren():
            if hasattr(child, 'getElementTree'):
                child.getElementTree(parent = elem)
            elif isinstance(child, (str, unicode)):
                elem.text = child
        return elem

    def _getName(self):
        """
        @return: the node's XML tag
        @rtype: C{str}
        """
        raise NotImplementedError()

    def _getLocalNamespaces(self):
        """
        @return: the locally defined namespaces
        @rtype: C{dict}
        """
        raise NotImplementedError()

    def _iterAttributes(self):
        """
        Iterate over this node's attributes.
        @return: iteratable of (attributeName, attributeValue)
        @rtype: iterable of (attributeName, attributeValue) strings
        """
        raise NotImplementedError()

    def _iterChildren(self):
        """
        Iterate over the node's children
        """
        raise NotImplementedError()

class _AbstractNode(SerializableObject):
    """Abstract node class for parsing XML data"""
    __slots__ = ['_children', '_nsMap', '_name', '_nsAttributes',
                 '_otherAttributes', ]
    _name = (None, None)

    def __init__(self, attributes = None, nsMap = None, name = None):
        SerializableObject.__init__(self)
        self._children = []
        self._nsMap = nsMap or {}
        self._nsAttributes = {}
        self._otherAttributes = {}
        self._setAttributes(attributes)
        if name is not None:
            self.setName(name)

    def setName(self, name):
        """Set the node's name"""
        nsName, tagName = splitNamespace(name)
        if nsName is not None and nsName not in self._nsMap:
            raise UndefinedNamespaceError(nsName)
        self._name = (nsName, tagName)
        return self

    def getName(self):
        """
        @return: the node's name
        @rtype: C{str}
        """
        return unsplitNamespace(self._name[1], self._name[0])

    def getAbsoluteName(self):
        """
        Retrieve the node's absolute name (qualified with the full
        namespace), in the format C{{namespace}node}
        @return: the node's absolute name
        @rtype: C{str}
        """
        if self._name[0] is None and None not in self._nsMap:
            # No default namespace provided
            return self._name[1]
        return "{%s}%s" % (self._nsMap[self._name[0]], self._name[1])

    def addChild(self, childNode):
        """
        Add a child node to this node
        @param childNode: Child node to be added to this node
        @type childNode: Node
        """
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
        "Iterate over this node's children"
        if hasattr(self, '_childOrder'):
            # pylint: disable-msg=E1101
            # no '_childOrder' member: we just tested for that
            return orderItems(self._children, self._childOrder)
        return iter(self._children)

    def finalize(self):
        "Post-process this node (e.g. cast text to the expected type)"
        return self

    def characters(self, ch):
        """Add character data to this node.
        @param ch: Character data to be added
        @type ch: C{str}
        @return: self
        @rtype: C{type(self)}
        """
        if self._children:
            if isinstance(self._children[-1], unicode):
                self._children[-1] += ch
            # We don't support mixed contents, so don't bother adding
            # characters after children
        else:
            self._children.append(ch)
        return self

    def getNamespaceMap(self):
        "Return a copy of the namespace mapping"
        return self._nsMap.copy()

    def iterAttributes(self):
        """
        Iterate over this node's attributes.
        @return: iterable of (attributeName, attributeValue)
        @rtype: iterable of (attributeName, attributeValue) strings
        """
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

    def iterNamespaces(self):
        """
        Iterate over this node's namespaces
        @return: iterable of (namespaceAlias, namespaceValue), with a
        C{namespaceAlias} equal to {None} for the default namespace.
        @rtype: iterable of (namespaceAlias, namespaceValue) strings
        """
        for nsName, nsVal in sorted(self._nsAttributes.items()):
            yield nsName, nsVal

    def getAttribute(self, name, namespace = None):
        """
        Get an attribute's value.
        @param name: the attribute name
        @type name: C{str}
        @param namespace: the namespace alias for the attribute (or None for
        the attribute with this name from the default namespace).
        @type namespace: C{str} or C{None}
        @return: the attribute's value
        @rtype: C{str}
        """
        return self._otherAttributes.get((namespace, name))

    def getAttributeByNamespace(self, name, namespace = None):
        """
        Retrieve an attribute using its full namespace designation
        @param name: the attribute name
        @type name: C{str}
        @param namespace: the full namespace for the attribute. Passing
        C{None} is equivalent to calling C{getAttribute} with the name and no
        namespace, i.e. the node's attribute will be returned only if no
        default namespace is set.
        @type namespace: C{str} or C{None}
        @return: the attribute's value
        @rtype: C{str}
        """
        if namespace is None:
            # Nothing different from getAttribute
            return self.getAttribute(name)

        # Get all aliases that correspond to this namespace
        aliases = [ x for (x, y) in self._nsMap.items() if y == namespace ]
        # Sort them (this way the default namespace comes first)
        aliases.sort()
        for alias in aliases:
            if (alias, name) in self._otherAttributes:
                return self._otherAttributes[(alias, name)]
        return None

    def getChildren(self, name, namespace = None):
        """
        Get a node's children, by name and (the optional) namespace.
        @param name: the attribute name
        @type name: C{str}
        @param namespace: the namespace alias for the attribute (or None for
        the attribute with this name from the default namespace).
        @type namespace: C{str} or C{None}
        @return: The children nodes with the specified name.
        @rtype: C{list}
        """
        tagName = unsplitNamespace(name, namespace)
        return [ x for x in self.iterChildren()
            if hasattr(x, 'getName') and x.getName() == tagName ]

    def getText(self):
        "Return a node's character data"
        text = [ x for x in self._children if isinstance(x, (str, unicode)) ]
        if not text:
            return ''
        return text[0]

    #{ Methods for serializing Node objects
    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _getName(self):
        if self._name[0] is None:
            return self._name[1]
        return "{%s}%s" % (self._nsMap[self._name[0]], self._name[1])

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _getLocalNamespaces(self):
        return self._nsAttributes

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _iterAttributes(self):
        for (nsName, attrName), attrVal in self._otherAttributes.items():
            attrName = self._buildElementTreeName(attrName, nsName)
            yield (attrName, attrVal)

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _iterChildren(self):
        return self.iterChildren()
    #}

    #{ Private methods
    def _setAttributes(self, attributes):
        "Set a node's attributes"
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
                raise UndefinedNamespaceError(nsName)
            self._otherAttributes[(nsName, attrName)] = attrVal

    def _buildElementTreeName(self, name, namespace = None):
        "Convenience function for building a namespace-qualified node name"
        if namespace is None:
            return name
        return "{%s}%s" % (self._nsMap[namespace], name)
    #}

class BaseNode(_AbstractNode):
    """Base node for parsing XML data"""
#}

#{ Specialized nodes
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
    _name = (None, 'int')

    def finalize(self):
        "Convert the character data to an integer"
        text = self.getText()
        try:
            return int(text)
        except ValueError:
            return 0

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _iterChildren(self):
        yield str(self.finalize())

class StringNode(BaseNode):
    """
    String data class for SAX parser.

    Registering a tag with this class will render the text contents into
    a string when finalize is called. All attributes and tags will be lost.
    If no text is set, this object will default to ''.
    """
    _name = (None, 'string')

    def finalize(self):
        "Convert the text data to a string"
        text = self.getText()
        return text

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _iterChildren(self):
        yield self.finalize()

class NullNode(BaseNode):
    """
    Null data class for SAX parser.

    Registering a tag with this class will render the text contents into
    None when finalize is called. All attributes and tags will be lost.
    All text will be lost.
    """
    _name = (None, 'none')

    def finalize(self):
        "Discard the character data"

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _iterChildren(self):
        return []

class BooleanNode(BaseNode):
    """
    Boolean data class for SAX parser.

    Registering a tag with this class will render the text contents into
    a bool when finalize is called. All attributes and tags will be lost.
    '1' or 'true' (case insensitive) will result in True.
    """
    _name = (None, 'bool')

    def finalize(self):
        "Convert the character data to a boolean value"
        text = self.getText()
        return self.fromString(text)

    @staticmethod
    def fromString(stringVal):
        """
        Convert character data to a boolean value
        @param stringVal: String value to be converted to boolean
        @type stringVal: C{str}
        @rtype: C{bool}
        """
        if isinstance(stringVal, bool):
            return stringVal
        return stringVal.strip().upper() in ('TRUE', '1')

    @staticmethod
    def toString(boolVal):
        """
        Convert boolean value to character data.
        @param boolVal: Boolean value to be converted
        @type boolVal: C{bool}
        @rtype: C{str}
        """
        return boolVal and "true" or "false"

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _iterChildren(self):
        yield self.toString(self.finalize())

#}


#{ Specialized objects that can be serialized
class SerializableList(list):
    """A List class that can be serialized to XML"""

    tag = None

    def getElementTree(self, parent = None):
        """Return a hierarchy of objects that represent the
        structure of the XML document.

        @param parent: An optional parent object.
        @type parent: C{SerializableObject} instance
        """
        elem = createElementTree(self._getName(), {}, {}, parent = parent)
        for child in self:
            child.getElementTree(parent = elem)
        return elem

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _getName(self):
        return self.tag

# pylint: disable-msg=R0903
# Too few public methods (1/2): this is an interface
class SlotBasedSerializableObject(SerializableObject):
    """
    A serializable object that uses the slots for defining the data that
    has to be serialized to XML
    """
    __slots__ = []
    tag = None

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _getName(self):
        return self.tag

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _getLocalNamespaces(self):
        return {}

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _iterAttributes(self):
        return self._splitData()[0].items()

    # pylint: disable-msg=C0111
    # docstring inherited from parent class
    def _iterChildren(self):
        return self._splitData()[1]

    def _splitData(self):
        """
        Split attributes and child nodes from the slots
        @return: A tuple (attributes, children)
        @rtype: C{tuple}
        """
        attrs = {}
        children = []
        for fName in self.__slots__:
            fVal = getattr(self, fName)
            if isinstance(fVal, (bool, int, str, unicode)):
                attrs[fName] = fVal
            elif fVal is None:
                # Skip None values
                continue
            elif isinstance(fVal, list):
                children.append(fVal)
            else:
                if not hasattr(fVal, "getElementTree"):
                    raise XmlLibError(
                        "Expected an object implementing getElementTree")
                children.append(fVal)
        return attrs, children
#}

#{ Binding classes
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
        """
        Register a class as a node handler.
        @param typeClass: A node class to register
        @type typeClass: C{class}
        @param name: the node name for which C{dispatch} will instantiate the
        node type class. If None, the name and namespace will be extracted by
        calling the class-level method C{getTag} of the node type class.
        @type name: C{str} or None
        @param namespace: An optional namespace
        @type namespace: C{str} or None
        """
        if name is None:
            name = typeClass.name
        if namespace is None:
            namespace = getattr(typeClass, 'namespace', None)

        self.typeDict[(namespace, name)] = typeClass

    def startElement(self, name, attrs):
        "SAX parser callback invoked when a start element event is emitted"
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
        "SAX parser callback invoked when an end element event is emitted"
        elem = self.stack.pop()
        assert(elem.getName() == name)
        if not self.stack:
            self.rootNode = elem.finalize()
        else:
            self.stack[-1].addChild(elem)

    def characters(self, ch):
        "SAX parser callback invoked when character data is found"
        elem = self.stack[-1]
        elem.characters(ch)


class DataBinder(object):
    """
    DataBinder class.

    This class wraps all XML parsing logic in this module.

    For the simple case, the binder can be used as-is, by invoking the
    C{parseString} method, in which case a hierarchy of C{BaseNode} objects
    will be produced. The top-level node can be also used for serializing
    back to XML

    EXAMPLE::

        binder = DataBinder()
        obj = binder.parseString('<baz><foo>3</foo><bar>test</bar></baz>')
        data = binder.toXml(prettyPrint = False)
        # data == '<baz><foo>3</foo><bar>test</bar></baz>'

    A node's attributes can be retrieved with C{iterAttributes}, and its
    children with C{iterChildren}. The node's text can be retrieved with
    C{getText}. Please note that we do not support character data and nodes
    mixed under the same parent. For insntance, the following XML construct
    will not be properly handled::

        <node>Some text<child>Child1</child>and some other text</node>

    To convert simple data types (like integer or boolean nodes) into their
    corresponding Python representation, you can use the built-in
    C{IntegerNode}, C{BooleanNode} and C{StringNode}. These objects implement
    a C{finalize} method that will convert the object's text into the proper
    data type (and ignore other possible child nodes).

    For a more convenient mapping of the data structures parsed from XML into
    rich Python objects, you can register your own classes, generally
    inherited from C{BaseNode}, that will overwrite the C{addChild} method to
    change the default behavior. You can choose to store children in a
    different way, instead of the default data structure of children.

    EXAMPLE::

        binder = xmllib.DataBinder()
        class MyClass(BaseNode):
            def addChild(self, child):
                if child.getName() == 'isPresent':
                    self.present = child.finalize()

        binder.registerType(MyClass, name = 'node')
        binder.registerType(BooleanNode, name = 'isPresent')

        obj = binder.parseString('<node><isPresent>true</isPresent></node>')
        # obj.isPresent == true

    parseFile: takes a a path and returns a python object.
    parseString: takes a string containing XML data and returns a python
    object.
    registerType: register a tag with a class defining how to treat XML content.
    toXml: takes an object and renders it into an XML representation.

    """
    def __init__(self, typeDict = None):
        """
        Initialize the Binder object.

        @param typeDict: optional type mapping object
        @type typeDict: dict
        """
        self.contentHandler = BindingHandler(typeDict)

    def registerType(self, klass, name = None, namespace = None):
        """
        Register a new class with a node name. As the XML parser encounters
        nodes, it will try to instantiate the classes registered with the
        node's name, and will fall back to the BaseNode class if a more
        specific class could not be found.

        If the optional keyword argument C{name} is not specified, the name
        of the node will be compared with the class' C{name} field (a
        class-level attribute).
        """
        return self.contentHandler.registerType(klass, name = name,
                                                namespace = namespace)

    def parseString(self, data):
        """
        Parse an XML string.
        @param data: the XML string to be parsed
        @type data: C{str}
        @return: a Node object
        @rtype: A previosly registered class (using C{registerType} or a
        C{BaseNode}.
        """
        stream = StringIO.StringIO(data)
        return self.parseFile(stream)

    def parseFile(self, stream):
        """
        Parse an XML file.
        @param stream: the XML file to be parsed
        @type stream: C{file}
        @return: a Node object
        @rtype: A previosly registered class (using C{registerType} or a
        C{BaseNode}.
        """
        if isinstance(stream, str):
            stream = file(stream)
        self.contentHandler.rootNode = None
        parser = sax.make_parser()
        parser.setContentHandler(self.contentHandler)
        parser.parse(stream)
        rootNode = self.contentHandler.rootNode
        self.contentHandler.rootNode = None
        return rootNode

    @classmethod
    def toXml(cls, obj, prettyPrint = True):
        """
        Serialize an object to XML.

        @param obj: An object implementing a C{getElementTree} mthod.
        @type obj: C{obj}
        @param prettyPrint: if True (the default), the XML that is produced
        will be formatted for easier reading by humans (by introducing new
        lines and white spaces).
        @type prettyPrint: C{bool}
        @return: the XML representation of the object
        @rtype: str
        """
        tree = obj.getElementTree()
        res = etree.tostring(tree, pretty_print = prettyPrint,
            xml_declaration = True, encoding = 'UTF-8')
        return res
#}

def splitNamespace(tag):
    """
    Splits the namespace out of the tag.
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
    """
    @param name: Name
    @type name: C{str}
    @param namespace: Namespace
    @type namespace: C{str} or None
    @return: the name qualified with the namespace
    @rtype: C{str}
    """
    if namespace is None:
        return name
    return "%s:%s" % (namespace, name)

def orderItems(items, order):
    """
    Reorder a list of items based on the order passed in as a list
    @param items:
    @type items: iterable
    @param order: list defining the items' ordering
    @type order: C{list}
    @return: the ordered list, unknown elements at the end
    @rtype: C{list}
    """
    # sort key is a three part tuple. each element maps to these rules:
    # element one reflects if we know how to order the element.
    # element two reflects the element's position in the ordering.
    # element three sorts everything else by simply providing the original
    # item (aka. default ordering of sort)
    orderHash = dict((y, i) for i, y in enumerate(order))
    return sorted(items,
        key = lambda x: (x.getName() not in orderHash,
                         orderHash.get(x.getName()),
                         x.getName()))

def createElementTree(name, attrs, nsMap = None, parent = None):
    """
    Create an element tree.

    @param name: Node name
    @type name: C{str}
    @param attrs: Node attributes
    @type attrs: C{dict}
    @param nsMap: A namespace alias to namespace mapping
    @type nsMap: C{dict}
    @param parent: An optional parent object.
    @type parent: C{etree.Element} instance
    @return: an element tree
    @rtype: C{etree.Element} instance
    """
    if nsMap is None:
        nsMap = {}
    if parent is not None:
        elem = etree.SubElement(parent, name, attrs, nsMap)
    else:
        elem = etree.Element(name, attrs, nsMap)
    return elem

class NodeDispatcher(object):
    """Simple class that dispatches nodes of various types to various
    registered classes.

    The registered classes need to implement a C{getTag()} static method or
    class method, that returns the name of the tags we want to be registered
    with this node.
    """

    def __init__(self, nsMap = None):
        """
        @param nsMap: a namespace mapping from aliases to the namespace
        string. For example, for the following declaration::

            <node xmlns="namespace1" xmlns:ns="namespace2"/>

        the mapping will be {None: "namespace1", "ns" : "namespace2"}
        @type nsMap: C{dict}
        """

        self._dispatcher = {}
        self._nsMap = nsMap or {}

    def registerType(self, typeClass, name = None, namespace = None):
        """
        Register a class as a node handler.
        @param typeClass: A node class to register
        @type typeClass: C{class}
        @param name: the node name for which C{dispatch} will instantiate the
        node type class. If None, the name and namespace will be extracted by
        calling the class-level method C{getTag} of the node type class.
        @type name: C{str} or None
        @param namespace: An optional namespace
        @type namespace: C{str} or None
        """
        if name is None:
            if not hasattr(typeClass, 'getTag'):
                return
            ns, name = splitNamespace(typeClass.getTag())
        else:
            ns, name = namespace, name

        key = "{%s}%s" % (self._nsMap.get(ns, ''), name)
        self._dispatcher[key] = typeClass

    def registerClasses(self, module, baseClass):
        """
        Register all classes that are a subclass of baseClass and are part
        of the module.
        @param module: The module in which supported classes will be looked up.
        @type module: module
        @param baseClass: A base class for all the classes that have to be
        registered.
        @type baseClass: class
        """
        for symVal in module.__dict__.itervalues():
            if not isinstance(symVal, type):
                continue
            if issubclass(symVal, baseClass) and symVal != baseClass:
                self.registerType(symVal)

    def dispatch(self, node):
        """
        Create objects for this node, based on the classes registered with
        the dispatcher.
        """

        absName = node.getAbsoluteName()
        if absName not in self._dispatcher:
            return None
        nodeClass = self._dispatcher.get(absName)

        return nodeClass(node)
