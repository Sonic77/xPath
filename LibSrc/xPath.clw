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


xPath.Search        PROCEDURE(STRING pxQuery)!, LONG, VIRTUAL
  CODE
  RETURN SELF.Search(SELF.xResults, SELF.xRoot, pxQuery)


xPath.Search        PROCEDURE(xObject pxRoot, STRING pxQuery)!, LONG, VIRTUAL
  CODE
  RETURN SELF.Search(SELF.xResults, pxRoot, pxQuery)
  
  
! TODO Nesting with rest of search query, using results parameter to find commands. idea; xResultObjects.SearchLevel to seperate query level results
  
xPath.Search        PROCEDURE(xResultObjects pxResults, <xObject pxRoot>, STRING pxQuery, LONG pSearchLevel=0)!, LONG, VIRTUAL
! https://www.w3schools.com/xml/xpath_syntax.asp
Obj                   &xObject
Results               &xResultObjects
PosBegin              LONG
PosEnd                LONG
I                     LONG
Count                 LONG

  CODE
  IF OMITTED(pxRoot) OR pxRoot &= NULL THEN
    Obj &= SELF.xRoot
  ELSE
    Obj &= pxRoot
  END
  IF Obj &= NULL THEN RETURN 0 END

  SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Begin: ' & pxQuery & ', Level=' & pSearchLevel)
  PosBegin = 1
!  PosBegin = INSTRING(xPathSeperator, pxQuery, 1, PosBegin)
!  IF    PosBegin = 0 THEN PosBegin = 1 
!  ELSIF PosBegin > 1 THEN PosBegin = 1 END
  IF LEN(pxQuery) >= 1 AND pxQuery[PosBegin] = xPathSeperator THEN
    IF LEN(pxQuery) >= 2 AND pxQuery[PosBegin + 1] = xPathSeperator THEN
      PosEnd = INSTRING(xPathSeperator, pxQuery, 1, PosBegin + 2)
    ELSE
      PosEnd = INSTRING(xPathSeperator, pxQuery, 1, PosBegin + 1)
    END
  END
  IF PosEnd = 0 THEN 
    PosEnd = LEN(pxQuery) 
  ELSE
    PosEnd -= 1
  END
  SELF.DebugOutput(ALL(' ', pSearchLevel * 2 + 1) & 'Search PosBegin=' & PosBegin & ', PosEnd=' & PosEnd & ', ' & SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1))
  
  IF    SUB(pxQuery, PosBegin, 2) = xPathParent THEN ! .. : Selects the parent of the current node
    PosBegin += 2
    IF NOT Obj.xParent &= NULL THEN
      SELF.AddNode(pxResults, Obj.xParent, 0)
      Count += 1
    END
  ELSIF SUB(pxQuery, PosBegin, 1) = xPathCurrent  THEN ! .  : Selects the current node
    PosBegin += 1
    SELF.AddNode(pxResults, Obj, 0)
    Count += 1
  ELSIF SUB(pxQuery, PosBegin, 2) = xPathAnywhere THEN ! // : Selects nodes in the document from the current node that match the selection no matter where they are 
    PosBegin += 2
    Count += SELF.FindAllChildNodes(pxResults, Obj, SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1), xPathSearch:NodeAndAttribute, True)
  ELSIF SUB(pxQuery, PosBegin, 1) = xPathRoot  THEN ! /  : Selects from the root node
    PosBegin += 1
    Count += SELF.FindAllChildNodes(pxResults, Obj, SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1), xPathSearch:NodeAndAttribute, False)
  ELSE                                        !    : Selects all nodes with the name
    Count += SELF.FindAllNodes(pxResults, Obj, SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1), xPathSearch:NodeAndAttribute)
  END
  IF RECORDS(pxResults) > 0 THEN
    !SELF.DebugOutput(ALL(' ', pSearchLevel * 2 + 1) & 'Search Set Level ' & pSearchLevel)
    LOOP I = 1 TO RECORDS(pxResults)
      GET(pxResults, I)
      !SELF.DebugOutput(ALL(' ', pSearchLevel * 2 + 1) & 'Search Set Level ' & pxResults.SearchLevel & ' => ' & pSearchLevel)
      IF pxResults.SearchLevel <> xPathSearchLevel:Unknown THEN CYCLE END
      pxResults.SearchLevel = pSearchLevel
      PUT(pxResults, I)
    END
  END
  IF PosEnd + 1 < LEN(pxQuery) THEN
    PosBegin = PosEnd + 1
    Count = 0
    Results &= NEW xResultObjects
    SELF.DebugOutput(ALL(' ', pSearchLevel * 2 + 1) & 'Search Next: ' & SUB(pxQuery, PosBegin, LEN(pxQuery) - PosEnd + 1))
    LOOP I = 1 TO RECORDS(pxResults)
      GET(pxResults, I)
      !IF pxResults.SearchLevel <> pSearchLevel THEN CYCLE END
      Count += SELF.Search(Results, pxResults.Obj, SUB(pxQuery, PosBegin, LEN(pxQuery) - PosEnd + 1), pSearchLevel + 1)
    END ! LOOP
    DISPOSE(SELF.xResults)
    SELF.xResults &= Results
  END
  SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search End: ' & pxQuery & ', Level=' & pSearchLevel)
    
  RETURN Count
  

xPath.FindAllNodes  PROCEDURE(STRING pNode, BYTE pPathSearch=xPathSearch:Node)!, LONG, VIRTUAL ! Searches all Nodes first to last
  CODE
  RETURN SELF.FindAllNodes(SELF.xResults, SELF.xRoot, pNode, pPathSearch)


xPath.FindAllNodes  PROCEDURE(xObject pxRoot, STRING pNode, BYTE pPathSearch=xPathSearch:Node)!, LONG, VIRTUAL ! Searches all Nodes first to last
  CODE
  RETURN SELF.FindAllNodes(SELF.xResults, pxRoot, pNode, pPathSearch)

  
xPath.FindAllNodes  PROCEDURE(xResultObjects pxResults, <xObject pxRoot>, STRING pNode, BYTE pPathSearch=xPathSearch:Node)!, LONG, VIRTUAL ! Searches all Nodes first to last
Obj                   &xObject
I                     LONG
Count                 LONG

  CODE
  IF OMITTED(pxRoot) OR pxRoot &= NULL THEN
    Obj &= SELF.xRoot
  ELSE
    Obj &= pxRoot
  END
  IF Obj &= NULL THEN RETURN 0 END
  !SELF.DebugOutput('FindAllNodes Begin')
  LOOP
    Count += SELF.MatchNode(pxResults, Obj, pNode, pPathSearch)
    Obj &= Obj.xNext
  UNTIL Obj &= NULL
  !SELF.DebugOutput('FindAllNodes End')
  RETURN Count
  
    
xPath.FindAllChildNodes PROCEDURE(STRING pNode, BYTE pPathSearch=xPathSearch:Node, BYTE pRecursive=True)!, LONG, VIRTUAL ! Searches Node and child Nodes
  CODE
  RETURN SELF.FindAllChildNodes(SELF.xResults, SELF.xRoot, pNode, pPathSearch, pRecursive)


xPath.FindAllChildNodes PROCEDURE(xObject pxRoot, STRING pNode, BYTE pPathSearch=xPathSearch:Node, BYTE pRecursive=True)!, LONG, VIRTUAL ! Searches Node and child Nodes
  CODE
  RETURN SELF.FindAllChildNodes(SELF.xResults, pxRoot, pNode, pPathSearch, pRecursive)
  
  
xPath.FindAllChildNodes PROCEDURE(xResultObjects pxResults, <xObject pxRoot>, STRING pNode, BYTE pPathSearch=xPathSearch:Node, BYTE pRecursive=True)!, LONG, VIRTUAL ! Searches Node and child Nodes
Obj                   &xObject
I                     LONG
Count                 LONG

  CODE
  IF OMITTED(pxRoot) OR pxRoot &= NULL THEN
    Obj &= SELF.xRoot
  ELSE
    Obj &= pxRoot
  END
  IF Obj &= NULL THEN RETURN 0 END
  !SELF.DebugOutput('FindAllChildNodes Begin in ' & Obj.ToString())
  Count += SELF.MatchNode(pxResults, Obj, pNode, pPathSearch)
  !SELF.DebugOutput('FindAllChildNodes Recursive? ' & pRecursive & ', ' & RECORDS(Obj.xChilderen))
  IF pRecursive = True AND NOT Obj.xChilderen &= NULL AND RECORDS(Obj.xChilderen) > 0 THEN
    LOOP I = 1 TO RECORDS(Obj.xChilderen)
      GET(Obj.xChilderen, I)
      Count += SELF.FindAllChildNodes(pxResults, Obj.xChilderen.Obj, pNode, pPathSearch, pRecursive)
    END
  END
  !SELF.DebugOutput('FindAllChildNodes End')
  RETURN Count

  
xPath.MatchNode     PROCEDURE(xObject pxObj, STRING pNode, BYTE pPathSearch=xPathSearch:Node)!, LONG, PRIVATE, VIRTUAL ! Searches Node
  CODE
  RETURN SELF.MatchNode(SELF.xResults, pxObj, pNode, pPathSearch)
  
  
xPath.MatchNode     PROCEDURE(xResultObjects pxResults, xObject pxObj, STRING pNode, BYTE pPathSearch=xPathSearch:Node)!, LONG, PRIVATE, VIRTUAL ! Searches Node
I                     LONG
Count                 LONG

  CODE
  IF pxObj &= NULL THEN RETURN 0 END
  IF pPathSearch = xPathSearch:None THEN RETURN 0 END
  !SELF.DebugOutput('MatchNode Begin ' & pNode & ' in ' & pxObj.ToString())
  IF pPathSearch = xPathSearch:Node OR pPathSearch = xPathSearch:NodeAndAttribute THEN
    IF pxObj.LengthTag() > 0 AND UPPER(pxObj.Tag) = UPPER(pNode) AND pxObj.IsClose() = False THEN
      SELF.DebugOutput('MatchNode found Node: ' & pxObj.ToString())
      SELF.AddNode(pxResults, pxObj, 0)
      Count += 1
    END
  END
  IF pPathSearch = xPathSearch:Attribute OR pPathSearch = xPathSearch:NodeAndAttribute THEN 
    I = pxObj.FindAttribute(pNode)
    IF I > 0 THEN
      SELF.DebugOutput('MatchNode found attribute ' & I & ': ' & pxObj.ToString())
      SELF.AddNode(pxResults, pxObj, I)
      Count += 1
    END
  END
  !SELF.DebugOutput('MatchNode End')
  RETURN Count
  
  
xPath.AddNode       PROCEDURE(xObject pxObj, LONG pIndex)!, PRIVATE, VIRTUAL ! Adds a Node to search results
  CODE
  SELF.AddNode(SELF.xResults, pxObj, pIndex)
  
  
xPath.AddNode       PROCEDURE(xResultObjects pxResults, xObject pxObj, LONG pIndex)!, PRIVATE, VIRTUAL ! Adds a Node to search results
  CODE
  IF pxResults &= NULL THEN RETURN END
  IF pxObj     &= NULL THEN RETURN END
  pxResults.Obj            &= pxObj
  pxResults.AttributeIndex  = pIndex
  pxResults.SearchLevel     = xPathSearchLevel:Unknown
  !SELF.DebugOutput('AddNode ' & pIndex & ': ' & pxObj.ToString())
  ADD(pxResults)


  
xPath.ToString      PROCEDURE()!, STRING, VIRTUAL
I                             LONG
X                             gcCString
  CODE
  IF SELF.xResults &= NULL THEN RETURN '' END
  IF RECORDS(SELF.xResults) = 0 THEN RETURN '' END
  X.Init(1024 * 1024)
  LOOP I = 1 TO RECORDS(SELF.xResults)
    GET(SELF.xResults, I)
    IF SELF.xResults.AttributeIndex = 0 THEN
      X.Value = X.Value & SELF.xResults.Obj.Tag & '<9>' & SELF.xResults.Obj.Contents & '<13,10>'
    ELSE
      X.Value = X.Value & SELF.xResults.Obj.GetAttributeLabel(SELF.xResults.AttributeIndex) & '<9>' & SELF.xResults.Obj.GetAttributeValue(SELF.xResults.AttributeIndex) & '<13,10>'
    END
  END
  RETURN X.Value


xPath.ToStringNodes PROCEDURE()!, STRING, VIRTUAL
I                             LONG
X                             gcCString
  CODE
  IF SELF.xResults &= NULL THEN RETURN '' END
  IF RECORDS(SELF.xResults) = 0 THEN RETURN '' END
  X.Init(1024 * 1024)
  LOOP I = 1 TO RECORDS(SELF.xResults)
    GET(SELF.xResults, I)
    X.Value = X.Value & SELF.xResults.Obj.ToString() & '<13,10>'
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


