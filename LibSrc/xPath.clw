  MEMBER

  MAP
    MODULE('')
      OutputDebugString(*CSTRING), PASCAL, RAW, NAME('OutputDebugStringA')
    END
  END

  INCLUDE('gcCString.inc'),ONCE
  INCLUDE('xObject.inc'),ONCE
  INCLUDE('xPath.inc'),ONCE


! Init


xPath.Construct    PROCEDURE!, PROTECTED
  CODE
  SELF.xRoot &= NULL
  SELF.xResults &= NEW xResultObjects


xPath.Destruct     PROCEDURE!, PROTECTED
I                     LONG
  CODE
  DISPOSE(SELF.xRoot)
!  IF NOT SELF.xResults &= NULL THEN ! No need, search results are always childs of root
!    LOOP I = 1 TO RECORDS(SELF.xResults)
!      GET(SELf.xResults, I)
!      DISPOSE(SELf.xResults.Obj)
!    END
!  END
  DISPOSE(SELF.xResults)
  

! Properties
  

xPath.Load          PROCEDURE(STRING pxStr)!, STRING, VIRTUAL
  CODE
  SELF.xRoot &= NEW xObject()
  SELF.xRoot.FromString(pxStr)


xPath.Free         PROCEDURE()!, VIRTUAL
  CODE
  FREE(SELf.xResults)

xPath.Search        PROCEDURE(<xObject pxRoot>, STRING pxQuery)!, LONG, VIRTUAL
! https://www.w3schools.com/xml/xpath_syntax.asp
Obj                   &xObject
PosBegin              LONG
PosEnd                LONG
Count                 LONG

  CODE
  IF NOT OMITTED(pxRoot) THEN
    Obj &= pxRoot
  ELSE
    Obj &= SELF.xRoot
  END
  IF Obj &= NULL THEN RETURN 0 END

  SELF.DebugOutput('Search Begin: ' & pxQuery)
  PosBegin = INSTRING('\', pxQuery, 1, 1)
  IF    PosBegin = 0 THEN PosBegin = 1 
  ELSIF PosBegin > 1 THEN PosBegin = 1 END
  PosEnd = INSTRING('\', pxQuery, 1, PosBegin)
  IF PosEnd = 0 THEN PosEnd = LEN(pxQuery) END
  
  IF    SUB(pxQuery, PosBegin, 2) = '..' THEN ! .. : Selects the parent of the current node
    PosBegin += 2
    IF NOT Obj.xParent &= NULL THEN
      SELF.AddTag(Obj.xParent, 0)
      Count += 1
    END
  ELSIF SUB(pxQuery, PosBegin, 1) = '.'  THEN ! .  : Selects the current node
    PosBegin += 1
    SELF.AddTag(Obj, 0)
    Count += 1
  ELSIF SUB(pxQuery, PosBegin, 2) = '//' THEN ! // : Selects nodes in the document from the current node that match the selection no matter where they are 
    PosBegin += 2
    Count += SELF.FindAllChildTag(Obj, SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1), xPathSearch:TagAndAttribute, True)
  ELSIF SUB(pxQuery, PosBegin, 1) = '/'  THEN ! /  : Selects from the root node
    PosBegin += 1
    Count += SELF.FindAllChildTag(Obj, SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1), xPathSearch:TagAndAttribute, False)
  ELSE                                        !    : Selects all nodes with the name
    Count += SELF.FindAllTag(Obj, SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1), xPathSearch:TagAndAttribute)
  END
  SELF.DebugOutput('Search End: ' & pxQuery)
    
  RETURN Count
  

xPath.FindAllTag    PROCEDURE(<xObject pxRoot>, STRING pTag, BYTE pPathSearch=xPathSearch:Tag)!, LONG, VIRTUAL ! Searches all Tags first to last
Obj                   &xObject
I                     LONG
Count                 LONG

  CODE
  IF NOT OMITTED(pxRoot) THEN
    Obj &= pxRoot
  ELSE
    Obj &= SELF.xRoot
  END
  IF Obj &= NULL THEN RETURN 0 END
  SELF.DebugOutput('FindAllTag Begin')
  LOOP
    Count += SELF.MatchTag(Obj, pTag, pPathSearch)
!    IF Obj.LengthTag() = 0 AND UPPER(Obj.Tag) = UPPER(pTag) AND Obj.IsClose() = False THEN
!      SELF.xResults.Obj   &= Obj
!      SELF.xResults.Index  = 0
!      SELF.DebugOutput('FindAllTag found: ' & SELF.xResults.Obj.ToString())
!      ADD(SELF.xResults)
!      Count += 1
!    END
!    IF pSearchAttributes = True THEN 
!      I = Obj.FindAttribute(pTag)
!      IF I > 0 THEN
!        SELF.xResults.Obj   &= Obj
!        SELF.xResults.Index  = I
!        SELF.DebugOutput('FindAllTag found attribute ' & I & ': ' & SELF.xResults.Obj.ToString())
!        ADD(SELF.xResults)
!        Count += 1
!      END
!    END
    Obj &= Obj.xNext
  UNTIL Obj &= NULL
  SELF.DebugOutput('FindAllTag End')
  RETURN Count
  
    
xPath.FindAllChildTag   PROCEDURE(<xObject pxRoot>, STRING pTag, BYTE pPathSearch=xPathSearch:Tag, BYTE pRecursive=True)!, LONG, VIRTUAL ! Searches tag and child tags
Obj                   &xObject
I                     LONG
Count                 LONG

  CODE
  IF NOT OMITTED(pxRoot) THEN
    Obj &= pxRoot
  ELSE
    Obj &= SELF.xRoot
  END
  IF Obj &= NULL THEN RETURN 0 END
  !SELF.DebugOutput('FindAllChildTag Begin in ' & Obj.ToString())
  Count += SELF.MatchTag(Obj, pTag, pPathSearch)
!  IF Obj.LengthTag() = 0 AND UPPER(Obj.Tag) = UPPER(pTag) THEN
!    SELF.xResults.Obj &= Obj
!    SELF.DebugOutput('FindAllTag found: ' & SELF.xResults.Obj.ToString())
!    ADD(SELF.xResults)
!    Count += 1
!  END
  !SELF.DebugOutput('FindAllChildTag Recursive? ' & pRecursive & ', ' & RECORDS(Obj.xChilderen))
  IF pRecursive = True AND NOT Obj.xChilderen &= NULL AND RECORDS(Obj.xChilderen) > 0 THEN
    LOOP I = 1 TO RECORDS(Obj.xChilderen)
      GET(Obj.xChilderen, I)
      Count += SELF.FindAllChildTag(Obj.xChilderen.Obj, pTag, pPathSearch, pRecursive)
    END
  END
  !SELF.DebugOutput('FindAllChildTag End')
  RETURN Count

  
xPath.MatchTag              PROCEDURE(xObject pxObj, STRING pTag, BYTE pPathSearch=xPathSearch:Tag)!, LONG, PRIVATE, VIRTUAL ! Searches tag
I                     LONG
Count                 LONG

  CODE
  IF pxObj &= NULL THEN RETURN 0 END
  IF pPathSearch = xPathSearch:None THEN RETURN 0 END
  !SELF.DebugOutput('MatchTag Begin ' & pTag & ' in ' & pxObj.ToString())
  IF pPathSearch = xPathSearch:Tag OR pPathSearch = xPathSearch:TagAndAttribute THEN
    IF pxObj.LengthTag() > 0 AND UPPER(pxObj.Tag) = UPPER(pTag) AND pxObj.IsClose() = False THEN
      SELF.DebugOutput('MatchTag found tag: ' & pxObj.ToString())
      SELF.AddTag(pxObj, 0)
      Count += 1
    END
  END
  IF pPathSearch = xPathSearch:Attribute OR pPathSearch = xPathSearch:TagAndAttribute THEN 
    I = pxObj.FindAttribute(pTag)
    IF I > 0 THEN
      SELF.DebugOutput('MatchTag found attribute ' & I & ': ' & pxObj.ToString())
      SELF.AddTag(pxObj, I)
      Count += 1
    END
  END
  !SELF.DebugOutput('MatchTag End')
  RETURN Count
  
  
xPath.AddTag        PROCEDURE(xObject pxObj, LONG pIndex)!, PRIVATE, VIRTUAL ! Adds a tag to search results
  CODE
  SELF.xResults.Obj   &= pxObj
  SELF.xResults.Index  = pIndex
  !SELF.DebugOutput('AddTag ' & pIndex & ': ' & pxObj.ToString())
  ADD(SELF.xResults)


  
xPath.ToString      PROCEDURE()!, STRING, VIRTUAL
I                             LONG
X                             gcCString
  CODE
  IF SELF.xResults &= NULL THEN RETURN '' END
  IF RECORDS(SELF.xResults) = 0 THEN RETURN '' END
  X.Init(1024 * 1024)
  LOOP I = 1 TO RECORDS(SELF.xResults)
    GET(SELF.xResults, I)
    X.Value = X.Value & SELF.xResults.Obj.ToString()
  END
  RETURN X.Value

  
! Getters

  
xPath.Records       PROCEDURE()!, LONG, VIRTUAL
  CODE
  IF SELF.xResults &= NULL THEN RETURN 0 END
  RETURN RECORDS(SELF.xResults)
  
  
xPath.Get           PROCEDURE(LONG pIndex)!, *xObject, VIRTUAL
  CODE
  IF SELF.xResults &= NULL THEN RETURN NULL END
  GET(SELF.xResults, pIndex)
  IF ERRORCODE() THEN RETURN NULL END
  RETURN SELF.xResults.Obj

  
! Debug

        
xPath.DebugOutput  PROCEDURE(STRING pMessage)!, VIRTUAL
Msg CSTRING(1024)
  CODE
  Msg = 'xPath: ' & pMessage ! & '<13,10>' & SELF.ToString()
  OutputDebugString(Msg)


