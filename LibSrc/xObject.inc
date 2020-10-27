!ABCIncludeFile

  OMIT('_EndOfInclude_', _xObject_)

_xObject_           EQUATE(1)


xObjectMaxLevels    EQUATE(99) ! Maximum level over tags for recursive parsing

xObjects            QUEUE()
Obj                   &xObject
                    END

xAttributes         QUEUE()
Label                 &CSTRING
Value                 &CSTRING
                    END


xObjectSeperator    EQUATE('/')


xObject             CLASS, TYPE, MODULE('xObject.CLW'), LINK('xObject.CLW', _ABCLinkMode_), DLL(_ABCDLLMode_)

! Data
Level                 LONG
Tag                   &CSTRING
!Attributes            &CSTRING
Attributes            &xAttributes
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
FindAttribute         PROCEDURE(<STRING pLabel>), LONG, VIRTUAL
!TODO: AddAttribute
!TODO: GetAttribute

! Properties
FromString            PROCEDURE(STRING pxStr), VIRTUAL

! Getters And Setters
SetLevel              PROCEDURE(LONG pLevel), VIRTUAL
SetTag                PROCEDURE(STRING pTag), VIRTUAL
GetTag                PROCEDURE(), STRING, VIRTUAL
SetAttributes         PROCEDURE(STRING pAttributes), VIRTUAL
SetAttributeLabel     PROCEDURE(STRING pLabel), VIRTUAL
GetAttributeLabel     PROCEDURE(LONG pIndex), STRING, VIRTUAL
SetAttributeValue     PROCEDURE(STRING pValue), VIRTUAL
GetAttributeValue     PROCEDURE(LONG pIndex), STRING, VIRTUAL
SetContents           PROCEDURE(STRING pContents), VIRTUAL
GetContents           PROCEDURE, STRING, VIRTUAL

! Information
ToStringChildren      PROCEDURE(), STRING, PRIVATE, VIRTUAL
ToStringAttributes    PROCEDURE(LONG pIndex=0), STRING, PRIVATE, VIRTUAL
ToStringContents      PROCEDURE(BYTE pRecursive=False), STRING, VIRTUAL
ToStringPath          PROCEDURE(<STRING pSeperator>), STRING, VIRTUAL
ToString              PROCEDURE(BYTE pRecursive=False), STRING, VIRTUAL
IsMeta                PROCEDURE(), BYTE, VIRTUAL ! e.g. <!DOCTYPE html>
IsComment             PROCEDURE(), BYTE, VIRTUAL ! e.g. <-- --/>
IsClosed              PROCEDURE(), BYTE, VIRTUAL ! e.g <br />
IsOpen                PROCEDURE(), BYTE, VIRTUAL ! e.g. <html>
IsClose               PROCEDURE(), BYTE, VIRTUAL ! e.g. </html>
IsNoNesting           PROCEDURE(), BYTE, VIRTUAL ! e.g. <script>
LengthTag             PROCEDURE(), LONG, VIRTUAL ! Length of tag string e.g. <p> -> 1
CountAttributes       PROCEDURE(), LONG, VIRTUAL ! Number of attributes e.g. <p class="ClassAttribute"> -> 1
LengthAttributes      PROCEDURE(), LONG, VIRTUAL ! Length of attributes string e.g. <p class="ClassAttribute"> -> 22
LengthContents        PROCEDURE(), LONG, VIRTUAL ! Length of contents string e.g. <p>contents</p> -> 8

! Helpers
RemoveBlanks          PROCEDURE(STRING pStr), STRING, VIRTUAL

! Debug
DebugOutput           PROCEDURE(STRING pMessage), VIRTUAL
                    END

  _EndOfInclude_

