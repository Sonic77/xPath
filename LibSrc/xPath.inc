!ABCIncludeFile

  OMIT('_EndOfInclude_', _xPath_)

_xPath_           EQUATE(1)


  INCLUDE('xObject.inc'),ONCE

xPathSearch         ITEMIZE, PRE(xPathSearch)
None                  EQUATE
Node                  EQUATE
Attribute             EQUATE
NodeAndAttribute      EQUATE
                    END

xPathSearchLevel    ITEMIZE, PRE(xPathSearchLevel)
Unknown               EQUATE(-1)
                    END


xResultObjects      QUEUE(xObjects)
AttributeIndex        LONG
SearchLevel           LONG
                    END


xPathSeperator      EQUATE('/')
xPathRoot           EQUATE('/')  ! /  : Selects from the root node
xPathAnywhere       EQUATE('//') ! // : Selects nodes in the document from the current node that match the selection no matter where they are 
xPathCurrent        EQUATE('.')  ! .  : Selects the current node
xPathParent         EQUATE('..') ! .. : Selects the parent of the current node


xPath               CLASS, TYPE, MODULE('xPath.CLW'), LINK('xPath.CLW', _ABCLinkMode_), DLL(_ABCDLLMode_)

! Data
xRoot                 &xObject
xResults              &xResultObjects

! Init
Construct             PROCEDURE, PROTECTED
Destruct              PROCEDURE, PROTECTED

! Properties
Load                  PROCEDURE(STRING pxStr), VIRTUAL
Free                  PROCEDURE(), VIRTUAL
Search                PROCEDURE(STRING pxQuery), LONG, VIRTUAL
Search                PROCEDURE(xObject pxRoot, STRING pxQuery), LONG, VIRTUAL
Search                PROCEDURE(xResultObjects pxResults, <xObject pxRoot>, STRING pxQuery, LONG pSearchLevel=0), LONG, VIRTUAL
FindAllNodes          PROCEDURE(STRING pNode, BYTE pPathSearch=xPathSearch:Node), LONG, VIRTUAL ! Searches all Nodes first to last
FindAllNodes          PROCEDURE(xObject pxRoot, STRING pNode, BYTE pPathSearch=xPathSearch:Node), LONG, VIRTUAL ! Searches all Nodes first to last
FindAllNodes          PROCEDURE(xResultObjects pxResults, <xObject pxRoot>, STRING pNode, BYTE pPathSearch=xPathSearch:Node), LONG, VIRTUAL ! Searches all Nodes first to last
FindAllChildNodes     PROCEDURE(STRING pNode, BYTE pPathSearch=xPathSearch:Node, BYTE pRecursive=True), LONG, VIRTUAL ! Searches Node and child Nodes
FindAllChildNodes     PROCEDURE(xObject pxRoot, STRING pNode, BYTE pPathSearch=xPathSearch:Node, BYTE pRecursive=True), LONG, VIRTUAL ! Searches Node and child Nodes
FindAllChildNodes     PROCEDURE(xResultObjects pxResults, <xObject pxRoot>, STRING pNode, BYTE pPathSearch=xPathSearch:Node, BYTE pRecursive=True), LONG, VIRTUAL ! Searches Node and child Nodes
MatchNode             PROCEDURE(xObject pxObj, STRING pNode, BYTE pPathSearch=xPathSearch:Node), LONG, PRIVATE, VIRTUAL ! Searches Node
MatchNode             PROCEDURE(xResultObjects pxResults, xObject pxObj, STRING pNode, BYTE pPathSearch=xPathSearch:Node), LONG, PRIVATE, VIRTUAL ! Searches Node
AddNode               PROCEDURE(xObject pxObj, LONG pIndex), PRIVATE, VIRTUAL ! Adds a Node to search results
AddNode               PROCEDURE(xResultObjects pxResults, xObject pxObj, LONG pIndex), PRIVATE, VIRTUAL ! Adds a Node to search results
ToString              PROCEDURE(), STRING, VIRTUAL

! Getters
Records               PROCEDURE(), LONG, VIRTUAL
Get                   PROCEDURE(LONG pIndex), *xObject, VIRTUAL

! Debug
DebugOutput           PROCEDURE(STRING pMessage), VIRTUAL
                    END

  _EndOfInclude_

