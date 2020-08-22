!ABCIncludeFile

  OMIT('_EndOfInclude_', _xObject_)

_xObject_           EQUATE(1)


FLOAT               EQUATE(REAL)

xObjectMaxLevels    EQUATE(99) ! Maximum level over tags for recursive parsing

xObjects            QUEUE()
Obj                   &xObject
                    END


xObject             CLASS, TYPE, MODULE('xObject.CLW'), LINK('xObject.CLW', _ABCLinkMode_), DLL(_ABCDLLMode_)

! Data
Level                 LONG
Tag                   &CSTRING
Attributes            &CSTRING
Contents              &CSTRING
_Closed               BYTE, PROTECTED
_Closing              BYTE, PROTECTED

xPrevious             &xObject
xNext                 &xObject
xParent               &xObject
xChilderen            &xObjects
xOpen                 &xObject ! If closeing tag link to open tag
xClose                &xObject ! If opening tag link to close tag

! Init
Construct             PROCEDURE, PROTECTED
Destruct              PROCEDURE, PROTECTED

! Relationships
FindParent            PROCEDURE(<STRING pxTag>), *xObject, VIRTUAL
AddChild              PROCEDURE(*xObject pxObj), VIRTUAL

! Properties
FromString            PROCEDURE(STRING pxStr), VIRTUAL
ToStringChildren      PROCEDURE(), STRING, PRIVATE, VIRTUAL
ToString              PROCEDURE(BYTE pRecursive=True), STRING, VIRTUAL
IsMeta                PROCEDURE(), BYTE, VIRTUAL ! e.g. <!DOCTYPE html>
IsComment             PROCEDURE(), BYTE, VIRTUAL ! e.g. <-- --/>
IsClosed              PROCEDURE(), BYTE, VIRTUAL ! e.g <br />
IsOpen                PROCEDURE(), BYTE, VIRTUAL ! e.g. <html>
IsClose               PROCEDURE(), BYTE, VIRTUAL ! e.g. </html>
IsNoNesting           PROCEDURE(), BYTE, VIRTUAL ! e.g. <script>
LengthTag             PROCEDURE(), LONG, VIRTUAL ! Length of tag string e.g. <p> -> 1
LengthAttributes      PROCEDURE(), LONG, VIRTUAL ! Length of attributes string e.g. <p class="ClassAttribute"> -> 22
LengthContents        PROCEDURE(), LONG, VIRTUAL ! Length of contents string e.g. <p>contents</p> -> 8

! Helpers
RemoveBlanks          PROCEDURE(STRING pStr), STRING, VIRTUAL

! Debug
DebugOutput           PROCEDURE(STRING pMessage), VIRTUAL
                    END

  _EndOfInclude_

