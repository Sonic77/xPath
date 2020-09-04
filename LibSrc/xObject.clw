  MEMBER

  MAP
    MODULE('')
      OutputDebugString(*CSTRING), PASCAL, RAW, NAME('OutputDebugStringA')
    END
  END

  INCLUDE('gcCString.inc'),ONCE
  INCLUDE('xObject.inc'),ONCE


! Init


xObject.Construct    PROCEDURE!, PROTECTED
  CODE
  SELF.xChilderen &= NEW xObjects
  SELF.Attributes &= NEW xAttributes
  SELF.xPrevious  &= NULL
  SELF.xParent    &= NULL
  SELF.xOpen      &= NULL
  SELF.Level       = 0

  
xObject.Destruct     PROCEDURE!, PROTECTED
I                       LONG
  CODE
  LOOP I = 1 TO RECORDS(SELF.xChilderen)
    GET(SELF.xChilderen, I)
    DISPOSE(SELF.xChilderen.Obj)
  END
  DISPOSE(SELF.xChilderen)
  IF NOT SELF.Tag &= NULL THEN 
    DISPOSE(SELF.Tag) 
  END
  LOOP I = 1 TO RECORDS(SELF.Attributes)
    GET(SELF.Attributes, I)
    DISPOSE(SELF.Attributes.Label)
    DISPOSE(SELF.Attributes.Value)
  END
  DISPOSE(SELF.Attributes)
  IF NOT SELF.Contents &= NULL THEN 
    DISPOSE(SELF.Contents) 
  END

  
! Relationships


xObject.FindParent  PROCEDURE(<STRING pxTag>)!, xObject, VIRTUAL
xParent               &xObject
  CODE
  xParent &= SELF.xParent
  LOOP WHILE NOT xParent &= NULL
    !SELF.DebugOutput('FindParent: ' & xParent.ToString(False) & '.') 
    IF xParent.Tag = pxTag THEN 
      !SELF.DebugOutput('FindParent: ' & xParent.ToString(False) & ' Match.') 
      RETURN xParent 
    END
    xParent &= xParent.xParent
  END
  !SELF.DebugOutput('FindParent: NULL.') 
  RETURN NULL


xObject.AddChild    PROCEDURE(*xObject pxObj)!, VIRTUAL
  CODE
  IF pxObj &= NULL THEN RETURN END
  SELF.xChilderen.Obj &= pxObj
  ADD(SELF.xChilderen)

  
xObject.FindAttribute   PROCEDURE(<STRING pLabel>)!, LONG, VIRTUAL
I                         LONG

  CODE
  IF SELF.Attributes &= NULL THEN RETURN '' END
  IF RECORDS(SELF.Attributes) = 0 THEN RETURN '' END
  LOOP I = 1 TO RECORDS(SELF.Attributes)
    GET(SELF.Attributes, I)
    IF NOT SELF.Attributes.Label &= NULL AND UPPER(SELF.Attributes.Label) = UPPER(pLabel) THEN
      RETURN I
    END
  END
  RETURN 0


! Properties
  

xObject.FromString  PROCEDURE(STRING pxStr)!, STRING, VIRTUAL
PosBegin              LONG
PosEnd                LONG
PosAttr               LONG

  CODE
  IF SELF.Level > xObjectMaxLevels THEN RETURN END ! Limit recursion depth
  IF LEN(pxStr) = 0 THEN RETURN END ! No Data ?
  !Parse data
  !SELF.DebugOutput('FromString Begin: ' & CHOOSE(LEN(pxStr) < 512, pxStr, SUB(pxStr, 1, 512) & '...'))
  ! Find Next Tag
  PosBegin = INSTRING('<', pxStr, 1, 1)
  IF PosBegin = 0 THEN RETURN END ! No more tags ?
  IF SUB(pxStr, PosBegin + 1, 1) = '/' AND NOT SELF.xPrevious &= NULL THEN
    SELF._Closing = True
    PosBegin += 1
  END
  PosEnd   = INSTRING('>', pxStr, 1, PosBegin + 1)
  ! Find Attributes and Slice Attributes
  PosAttr  = INSTRING(' ', pxStr, 1, PosBegin + 1)
  !SELF.DebugOutput('FromString PosBegin=' & PosBegin & ', PosEnd=' & PosEnd & ', PosAttr=' & PosAttr & '.')
  IF PosAttr = 0 OR PosAttr > PosEnd THEN PosAttr = PosEnd END ! No extra data in tag?
  IF PosAttr < PosEnd THEN
    IF SUB(pxStr, PosEnd -1, 1) = '/' THEN
      SELF._Closed = True
      SELF.SetAttributes(SUB(pxStr, PosAttr + 1, PosEnd - PosAttr - 1 - 1))
    ELSE
      SELF.SetAttributes(SUB(pxStr, PosAttr + 1, PosEnd - PosAttr - 1))
    END
  END
  ! Slice Tag
  SELF.SetTag(SUB(pxStr, PosBegin + 1, PosAttr - PosBegin - 1))

  ! Find Contents
  IF SELF.IsNoNesting() THEN ! Must find end tag, no nesting allowed
    PosBegin = INSTRING('</', pxStr, 1, PosEnd + 1)
!    SELF.DebugOutput('FromString NoNesting ' & PosBegin & '.')
  ELSE
!    SELF.DebugOutput('FromString Nesting ' & PosBegin & '.')
    PosBegin = INSTRING('<', pxStr, 1, PosEnd + 1)
    IF PosBegin = 0 THEN PosBegin = LEN(pxStr) END ! Als consider data after last tag as contents
  END
  IF PosBegin > posEnd + 1 THEN
!    SELF.DebugOutput('FromString Contents ' & PosBegin - PosEnd & '.')
    SELF.SetContents(SUB(pxStr, PosEnd + 1, posBegin - PosEnd - 1))
    PosEnd = PosBegin - 1
  END

  ! Create Child and Parse remaining data, all objects are linked to next and previous objects
  SELF.xNext &= NEW xObject
  SELF.xNext.xPrevious &= SELF
  IF SELF.IsMeta() OR SELF.IsComment() OR SELF.IsClosed() THEN ! Tags not needing closeing are added to the current object level
    !SELF.DebugOutput('FromString Meta/Comment/Closed') 
    SELF.xNext.xParent    &= SELF.xParent
    SELF.xNext.Level       = SELF.Level
  ELSIF SELF.IsClose() THEN ! Closing Tags with Open tag then find matching open tag to be the parent
    !SELF.DebugOutput('FromString Closed') 
    SELF.xOpen            &= SELF.FindParent(SELF.Tag)
    IF NOT SELF.xOpen &= NULL THEN ! Opening tag found
      SELF.xOpen.xClose     &= SELF
      SELF.xParent          &= SELF.xOpen
    ELSE ! Opening tag not found
      SELF.xParent          &= SELF.xParent
    END
    IF NOT SELF.xParent &= NULL THEN
      SELF.xNext.xParent    &= SELF.xParent.xParent
      SELF.xNext.Level       = SELF.xParent.Level
    END
  ELSE ! Normal / Other Tags
    !SELF.DebugOutput('FromString Other') 
    SELF.xNext.xParent    &= SELF
    SELF.xNext.Level       = SELF.Level + 1
  END
  IF NOT SELF.xParent &= NULL THEN ! Link parent to child
    SELF.xParent.AddChild(SELF)
  END
  IF SELF.xParent &= NULL AND NOT SELF.xPrevious &= NULL THEN ! When no parent (root level) child to previous tag
    SELF.xPrevious.AddChild(SELF)
    SELF.xParent          &= SELF.xPrevious
    SELF.Level             = SELF.xPrevious.Level + 1
  END

  SELF.DebugOutput('FromString: ' & CHOOSE(SELF._Closed = True, 'Closed/', CHOOSE(SELF._Closing, '/Closing', 'Opening')) & | 
    CHOOSE(SELF.LengthTag() > 0, ', Tag=''' & SELF.Tag & '''', '') & | 
    CHOOSE(NOT SELF.xParent &= NULL, ', Parent=''' & SELF.xParent.Tag & '''', '') & | 
    CHOOSE(NOT SELF.xPrevious &= NULL, ', Previous=''' & SELF.xPrevious.Tag & '''', '') & | 
    CHOOSE(SELF.LengthAttributes() > 0, ', Attributes=''' & SELF.ToStringAttributes() & '''', '') & | 
    CHOOSE(SELF.LengthContents() > 200, ' , Contents=''' & SUB(SELF.Contents, 1, 100) & '....' & SUB(SELF.Contents, -100, 100) & '''', CHOOSE(SELF.LengthContents() > 0, ' , Contents=''' & SELF.Contents & '''', '')) & '.')

  !SELF.DebugOutput('FromString Next:' & CHOOSE(LEN(pxStr) - PosEnd < 512, SUB(pxStr, PosEnd + 1, LEN(pxStr) - PosEnd), SUB(pxStr, PosEnd + 1, 512) & '...'))
  SELF.xNext.FromString(SUB(pxStr, PosEnd + 1, LEN(pxStr) - PosEnd))
  
  
! Setters
  
  
xObject.SetLevel    PROCEDURE(LONG pLevel)!, VIRTUAL
  CODE
  SELF.Level = pLevel
  
  
xObject.SetTag      PROCEDURE(STRING pTag)!, VIRTUAL
  CODE
  IF NOT SELF.Tag &= NULL THEN 
    DISPOSE(SELF.Tag) 
  END
  SELF.Tag &= NEW CSTRING(LEN(pTag) + 1)
  SELF.Tag = pTag & '<0>'

  
xObject.SetAttributes   PROCEDURE(STRING pAttributes)!, VIRTUAL
PosBegin                  LONG
PosBegin2                 LONG
PosEnd                    LONG
Quot                      STRING(1)

  CODE
  !SELF.Attributes &= NEW CSTRING(LEN(pAttributes) + 1)
  !SELF.Attributes = pAttributes & '<0>'
  ! Find Label
  !SELF.DebugOutput('SetAttributes Begin: ' & CHOOSE(LEN(pAttributes) < 512, pAttributes, SUB(pAttributes, 1, 512) & '...'))
  PosBegin = 1
  LOOP
    IF PosBegin >= LEN(pAttributes) THEN BREAK END
    PosEnd = INSTRING('=', pAttributes, 1, PosBegin)
    IF PosEnd = 0 THEN PosEnd = LEN(pAttributes) + 1 END
    IF PosEnd = PosBegin THEN BREAK END
    SELF.SetAttributeLabel(SUB(pAttributes, PosBegin, PosEnd - PosBegin))
    ! Find Value
    PosBegin  = INSTRING('"', pAttributes, 1, PosEnd + 1)
    PosBegin2 = INSTRING('''', pAttributes, 1, PosEnd + 1)
    IF PosBegin < PosBegin2 THEN 
      PosBegin = PosBegin2 
      Quot = ''''
    ELSE
      Quot = '"'
    END
    IF PosBegin > 0 THEN 
      PosEnd = INSTRING(Quot, pAttributes, 1, PosBegin + 1)
      IF PosEnd = 0 THEN PosEnd = LEN(pAttributes) END
      PosEnd += 1
      SELF.SetAttributeValue(SUB(pAttributes, PosBegin, PosEnd - PosBegin))
      PosBegin = PosEnd + 1
    END
    ADD(SELF.Attributes)
    CLEAR(SELF.Attributes)
    IF PosBegin = 0 THEN BREAK END
  END

  
xObject.SetAttributeLabel   PROCEDURE(STRING pLabel)!, VIRTUAL
  CODE
  IF SELF.Attributes &= NULL THEN 
    RETURN
  END
  IF NOT SELF.Attributes.Label &= NULL THEN 
    DISPOSE(SELF.Attributes.Label) 
  END
  SELF.Attributes.Label &= NEW CSTRING(LEN(pLabel) + 1)
  SELF.Attributes.Label = pLabel & '<0>'
  !SELF.DebugOutput('SetAttribute Label=' & SELF.Attributes.Label & '.')

  
xObject.SetAttributeValue   PROCEDURE(STRING pValue)!, VIRTUAL
  CODE
  IF SELF.Attributes &= NULL THEN 
    RETURN
  END
  IF NOT SELF.Attributes.Value &= NULL THEN 
    DISPOSE(SELF.Attributes.Value) 
  END
  SELF.Attributes.Value &= NEW CSTRING(LEN(pValue) + 1)
  SELF.Attributes.Value = pValue & '<0>'
  !SELF.DebugOutput('SetAttribute Value=' & SELF.Attributes.Value & '.')

  
xObject.SetContents PROCEDURE(STRING pContents)!, VIRTUAL
  CODE
  IF NOT SELF.Contents &= NULL THEN 
    DISPOSE(SELF.Contents) 
  END
  SELF.Contents &= NEW CSTRING(LEN(pContents) + 1)
  !SELF.Contents = pContents & '<0>'
  SELF.Contents = SELF.RemoveBlanks(pContents) & '<0>'
  
  
! Getters

  
xObject.ToStringAttributes  PROCEDURE()!, STRING, PRIVATE, VIRTUAL
I                             LONG
X                             gcCString
  CODE
  IF SELF.Attributes &= NULL THEN RETURN '' END
  IF RECORDS(SELF.Attributes) = 0 THEN RETURN '' END
  !SELF.DebugOutput('ToStringAttributes Records=' & RECORDS(SELF.Attributes))
  X.Init(1024 * 1024)
  LOOP I = 1 TO RECORDS(SELF.Attributes)
    GET(SELF.Attributes, I)
    IF I > 1 THEN 
      X.Value = X.Value & ' '
    END
    IF NOT SELF.Attributes.Label &= NULL THEN
      X.Value = X.Value & SELF.Attributes.Label
    END
    IF NOT SELF.Attributes.Value &= NULL THEN
      X.Value = X.Value & '=' & SELF.Attributes.Value
    END
  END
  RETURN X.Value

    
xObject.ToStringChildren    PROCEDURE()!, STRING, PRIVATE, VIRTUAL
I                             LONG
X                             gcCString
  CODE
  IF SELF.xChilderen &= NULL THEN RETURN '' END
  IF RECORDS(SELF.xChilderen) = 0 THEN RETURN '' END
  X.Init(1024 * 1024)
  LOOP I = 1 TO RECORDS(SELF.xChilderen)
    GET(SELF.xChilderen, I)
    X.Value = X.Value & SELF.xChilderen.Obj.ToString()
  END
  RETURN X.Value

  
xObject.ToString    PROCEDURE(BYTE pRecursive=False)!(BYTE pRecursive=True), STRING, VIRTUAL
  CODE
  RETURN '<' & CHOOSE(SELF._Closing, '/', '') & CHOOSE(SELF.LengthTag() > 0, SELF.Tag, '') & | 
         CHOOSE(SELF.LengthAttributes() > 0, ' ' & SELF.ToStringAttributes(), '') & CHOOSE(SELF._Closed, '/', '') & '>' & | 
         |!CHOOSE(SELF.LengthContents() > 0, SELF.Contents, '') & |
         CHOOSE(SELF.LengthContents() > 0, SELF.Contents, '') & |
         CHOOSE(pRecursive = True, SELF.ToStringChildren(), CHOOSE(SELF.xClose &= NULL, '', SELF.xClose.ToString(False)))
  
  
xObject.IsMeta      PROCEDURE()!, BYTE, VIRTUAL
  CODE
  IF SELF.Tag &= NULL THEN
    RETURN False
  ELSIF SUB(SELF.Tag, 1, 1) = '!' THEN
    RETURN True
  ELSE
    RETURN False
  END
  

xObject.IsComment   PROCEDURE()!, BYTE, VIRTUAL
  CODE
  IF SELF.Tag &= NULL THEN
    RETURN False
  ELSIF SUB(SELF.Tag, 1, 3) = '!--' THEN
    RETURN True
  ELSE
    RETURN False
  END

  
xObject.IsClosed    PROCEDURE()!, BYTE, VIRTUAL
  CODE
  !IF SELF.Attributes &= NULL THEN
  !  RETURN False
  !ELSIF SUB(SELF.Attributes, -1, 1) = '/' THEN
  !  RETURN True
  !ELSE
  !  RETURN False
  !END
  RETURN SELF._Closed

  
xObject.IsOpen      PROCEDURE()!, BYTE, VIRTUAL ! e.g. <html>
  CODE
  IF SELF.Tag &= NULL THEN
    RETURN False
  ELSIF SUB(SELF.Tag, 1, 1) = '/' THEN ! IsClose()=True
    RETURN False
  ELSIF SUB(SELF.Tag, 1, 1) = '!' THEN ! IsMeta()=True
    RETURN False
  ELSIF SUB(SELF.Tag, 1, 2) = '--' THEN ! IsComment()=True
    RETURN False
  ELSE
    RETURN True
  END

  
xObject.IsClose     PROCEDURE()!, BYTE, VIRTUAL ! e.g. </html>
  CODE
  !IF SELF.Tag &= NULL THEN
  !  RETURN False
  !ELSIF SUB(SELF.Tag, 1, 1) = '/' THEN
  !  RETURN True
  !ELSE
  !  RETURN False
  !END
  RETURN SELF._Closing

  
xObject.IsNoNesting PROCEDURE()!, BYTE, VIRTUAL ! e.g. <script>
  CODE
  IF SELF.Tag &= NULL THEN
    RETURN False
  ELSIF SELF._Closing OR SELF._Closed THEN
    RETURN False
  ELSIF LOWER(SELF.Tag) = 'script' THEN
    RETURN True
  ELSE
    RETURN False
  END


xObject.LengthTag   PROCEDURE()!, LONG, VIRTUAL ! Length of tag string e.g. <p> -> 1
  CODE
  IF SELF.Tag &= NULL THEN
    RETURN 0
  ELSE
    RETURN LEN(SELF.Tag)
  END

  
xObject.LengthAttributes    PROCEDURE()!, LONG, VIRTUAL ! Length of string e.g. <p class="ClassAttribute"> -> 22
I                             LONG
X                             LONG
  CODE
  IF SELF.Attributes &= NULL THEN RETURN 0 END
  X = 0
  LOOP I = 1 TO RECORDS(SELF.Attributes)
    GET(SELF.Attributes, I)
    IF I > 1 THEN X += 1 END
    X = X + LEN(SELF.Attributes.Label) + 1 +  LEN(SELF.Attributes.Value)
  END
  RETURN X



xObject.LengthContents  PROCEDURE()!, LONG, VIRTUAL ! Length of string e.g. <p>contents</p> -> 8
  CODE
  IF SELF.Contents &= NULL THEN
    RETURN 0
  ELSE
    RETURN LEN(SELF.Contents)
  END

  
xObject.GetAttributeLabel   PROCEDURE(LONG pIndex)!, STRING, VIRTUAL
  CODE
  IF SELF.Attributes &= NULL THEN RETURN '' END
  GET(SELF.Attributes, pIndex)
  IF ERRORCODE() THEN RETURN '' END
  IF SELF.Attributes.Label &= NULL THEN RETURN '' END
  RETURN SELF.Attributes.Label

  
xObject.GetAttributeValue   PROCEDURE(LONG pIndex)!, STRING, VIRTUAL
  CODE
  IF SELF.Attributes &= NULL THEN RETURN '' END
  GET(SELF.Attributes, pIndex)
  IF ERRORCODE() THEN RETURN '' END
  IF SELF.Attributes.Value &= NULL THEN RETURN '' END
  RETURN SELF.Attributes.Value


! Helpers
  
  
xObject.RemoveBlanks    PROCEDURE(STRING pStr)!, STRING, VIRTUAL
S                         LONG ! Source Index
D                         LONG ! Destination Index
Ret                       gcCString
  CODE
  Ret.Init(LEN(pStr) + 1)
  !SELF.DebugOutput('RemoveBlanks(' & LEN(pStr) & '/' & SIZE(pStr) & ')')
  D = 0
  LOOP S = 1 TO LEN(pStr)
    IF VAL(pStr[S]) <= 27 THEN CYCLE END ! No special (escape) characters
    IF VAL(pStr[S])  = 32 AND D > 1 AND VAL(Ret.Value[D]) = 32 THEN CYCLE END ! No duplicate space
    D += 1
    Ret.Value[D] = pStr[S]
  END
  Ret.Value[D + 1] = '<0>'
!  RETURN ''
  RETURN Ret.Value

  
! Debug

        
xObject.DebugOutput  PROCEDURE(STRING pMessage)!, VIRTUAL
Msg CSTRING(1024)
  CODE
  Msg = ALL(' ', SELF.Level) & 'xObject: ' & pMessage ! & '<13,10>' & SELF.ToString()
  OutputDebugString(Msg)


