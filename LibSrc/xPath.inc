!ABCIncludeFile

  OMIT('_EndOfInclude_', _xPath_)

_xPath_           EQUATE(1)


  INCLUDE('xObject.inc'),ONCE

xPathSearch         ITEMIZE, PRE(xPathSearch)
None                  EQUATE
Tag                   EQUATE
Attribute             EQUATE
TagAndAttribute       EQUATE
                    END


xResultObjects      QUEUE(xObjects)
Index                 LONG
                    END


xPath               CLASS, TYPE, MODULE('xPath.CLW'), LINK('xPath.CLW', _ABCLinkMode_), DLL(_ABCDLLMode_)
! TODO: Rename Tag->Node

! Data
xRoot                 &xObject
xResults              &xResultObjects

! Init
Construct             PROCEDURE, PROTECTED
Destruct              PROCEDURE, PROTECTED

! Properties
Load                  PROCEDURE(STRING pxStr), VIRTUAL
Free                  PROCEDURE(), VIRTUAL
Search                PROCEDURE(<xObject pxRoot>, STRING pxQuery), LONG, VIRTUAL
FindAllTag            PROCEDURE(<xObject pxRoot>, STRING pTag, BYTE pPathSearch=xPathSearch:Tag), LONG, VIRTUAL ! Searches all Tags first to last
FindAllChildTag       PROCEDURE(<xObject pxRoot>, STRING pTag, BYTE pPathSearch=xPathSearch:Tag, BYTE pRecursive=True), LONG, VIRTUAL ! Searches tag and child tags
MatchTag              PROCEDURE(xObject pxObj, STRING pTag, BYTE pPathSearch=xPathSearch:Tag), LONG, PRIVATE, VIRTUAL ! Searches tag
AddTag                PROCEDURE(xObject pxObj, LONG pIndex), PRIVATE, VIRTUAL ! Adds a tag to search results
ToString              PROCEDURE(), STRING, VIRTUAL

! Getters
Records               PROCEDURE(), LONG, VIRTUAL
Get                   PROCEDURE(LONG pIndex), *xObject, VIRTUAL

! Debug
DebugOutput           PROCEDURE(STRING pMessage), VIRTUAL
                    END

  _EndOfInclude_

