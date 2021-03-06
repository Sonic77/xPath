  MEMBER

  MAP
    MODULE('')
      OutputDebugString(*CSTRING), PASCAL, RAW, NAME('OutputDebugStringA')
    END
  END

  INCLUDE('FormulaTranslator.inc'),ONCE
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
  

!!! Loads current object with data from supplied xml/html document and creates nested child tags for everything
xPath.Load          PROCEDURE(STRING pxStr)!, STRING, VIRTUAL
  CODE
  SELF.xRoot &= NEW xObject()
  SELF.xRoot.FromString(pxStr)

  
!!! Clears the current result set
xPath.Free         PROCEDURE()!, VIRTUAL
  CODE
  FREE(SELf.xResults)


!!! Searches for results that can be found usign the specified query in the current results set and starting from the current root
xPath.Search        PROCEDURE(STRING pxQuery)!, LONG, VIRTUAL
  CODE
  RETURN SELF.Search(SELF.xResults, SELF.xRoot, pxQuery, 0, 1, 1)


!!! Searches for results that can be found usign the specified query in the current results set and starting from the specified root
xPath.Search        PROCEDURE(xObject pxRoot, STRING pxQuery)!, LONG, VIRTUAL
  CODE
  RETURN SELF.Search(SELF.xResults, pxRoot, pxQuery, 0, 1, 1)
  

xPath.Search        PROCEDURE(xResultObjects pxResults, xObject pxRoot, STRING pxQuery)!, LONG, VIRTUAL
  CODE
  RETURN SELF.Search(pxResults, pxRoot, pxQuery, 0, 1, 1)

  
!!! Searches for results that can be found usign the specified query in the specified results set and starting from the specified root, level is used in the recursive search call
xPath.Search        PROCEDURE(xResultObjects pxResults, xObject pxRoot, STRING pxQuery, LONG pSearchLevel, LONG pSearchIndex, LONG pSearchCount)!, LONG, PROTECTED, VIRTUAL
! https://www.w3schools.com/xml/xpath_syntax.asp
Obj                   &xObject
Results               &xResultObjects
ForTran               &FormulaTranslator
PosBegin              LONG
PosPart               LONG
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
  SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' Begin: ''' & pxQuery & ''', Root=' & Obj.ToString())

  ! Check if multiple search queries
  PosBegin = INSTRING(xPathMultiple, pxQuery, -1, LEN(pxQuery))
  IF PosBegin = 0 THEN
    PosBegin = 1
    LOOP WHILE PosBegin < LEN(pxQuery) AND pxQuery[PosBegin] = ' '  ! Skip spaces at beginning
      PosBegin += 1
    END
  ELSE ! Seperate Search call for multiple queries
    !SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search before multiple Level=' & pSearchLevel & ' Begin: ''' & SUB(pxQuery, 1, PosBegin - 1) & ''', Root=' & Obj.ToString())
    Count += SELF.Search(pxResults, Obj, SUB(pxQuery, 1, PosBegin - 1), pSearchLevel, 1, 1)
    PosBegin += 1
    LOOP WHILE PosBegin < LEN(pxQuery) AND pxQuery[PosBegin] = ' '  ! Skip spaces at beginning
      PosBegin += 1
    END
    !SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search after multiple Level=' & pSearchLevel & ' Begin: ''' & SUB(pxQuery, PosBegin, LEN(pxQuery) - PosBegin) & ''', Root=' & Obj.ToString())
  END
  
  ! Get end of current part of search query
  IF LEN(pxQuery) >= 1 AND pxQuery[PosBegin] = xPathSeperator THEN ! Absolute? \
    IF LEN(pxQuery) >= 2 AND pxQuery[PosBegin + 1] = xPathSeperator THEN ! Relative? \\
      PosEnd = INSTRING(xPathSeperator, pxQuery, 1, PosBegin + 2)
    ELSE
      PosEnd = INSTRING(xPathSeperator, pxQuery, 1, PosBegin + 1)
    END
    IF PosEnd > 0 THEN ! Seperator then cut before
      PosEnd -= 1
    END
  ELSIF LEN(pxQuery) >= 1 AND pxQuery[PosBegin] = xPathPredicateBegin THEN ! Predicate? [
    PosBegin += 1
    PosEnd = INSTRING(xPathPredicateEnd, pxQuery, 1, PosBegin ) ! Predicate? ]
    IF PosEnd <= 0 THEN ! No predicate end ] found then accept seperator \
      PosEnd = INSTRING(xPathSeperator, pxQuery, 1, PosBegin ) ! Seperator? \
    END
    
    ForTran  &= NEW FormulaTranslator    
    ForTran.Parse(SUB(pxQuery, PosBegin, PosEnd - PosBegin))
    
    
  END
  IF PosEnd <= 0 THEN ! No seperator = Last term
    PosEnd = LEN(pxQuery)
  END
  
  ! Find Predicates after this term
  PosPart = INSTRING(xPathPredicateBegin, pxQuery, 1, PosBegin + 1) ! []
  SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' PosBegin=' & PosBegin & ', PosEnd=' & PosEnd & ', PosPart=' & PosPart & ', ''' & SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1) & '''')
  IF PosPart > PosEnd THEN
    PosPart = 0
  ELSIF PosPart > 0 THEN
    PosEnd = PosPart - 1
  END
  SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' PosBegin=' & PosBegin & ', PosEnd=' & PosEnd & ', PosPart=' & PosPart & ', ''' & SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1) & '''')

  ! Execute first part of search query
  !IF PosPart <= 0 THEN ! No Predicate? []
  IF SUB(pxQuery, PosBegin, 1) <> xPathPredicateBegin THEN ! No Predicate? []
    IF    SUB(pxQuery, PosBegin, 2) = xPathParent THEN   ! .. : Selects the parent of the current node
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
      Count += SELF.FindAllChildNodes(pxResults, Obj, SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1), xPathSearch:Node, True)
    ELSIF SUB(pxQuery, PosBegin, 1) = xPathRoot  THEN    ! /  : Selects from the root node
      PosBegin += 1
      Count += SELF.FindAllChildNodes(pxResults, Obj, SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1), xPathSearch:Node, False)
    ELSE                                                 !    : Selects all nodes with the name
      Count += SELF.FindAllNodes(pxResults, Obj, SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1), xPathSearch:Node)
    END
! ???
!  ELSE ! Predicate? []
!    IF NUMERIC(SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1)) THEN ! number : Select only specific result index
!      IF pSearchIndex = SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1) THEN
!        SELF.AddNode(pxResults, Obj, 0)
!        Count += 1
!      END
!    ELSIF UPPER(SUB(pxQuery, PosBegin, PosEnd - PosBegin + 1)) = UPPER(xPathIndexLast) THEN
!      IF pSearchIndex = pSearchCount THEN
!        SELF.AddNode(pxResults, Obj, 0)
!        Count += 1
!      END
!    END
  END
  
  ! Set correct search level, index, count values
  IF RECORDS(pxResults) > 0 THEN
    !SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' Set Level')
    LOOP I = 1 TO RECORDS(pxResults)
      GET(pxResults, I)
      IF pxResults.SearchLevel <> xPathSearchLevel:Unknown THEN CYCLE END
      !SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' Set Level ' & pxResults.SearchLevel)
      pxResults.SearchLevel = pSearchLevel
      pxResults.Index = I
      pxResults.Count = RECORDS(pxResults)
      PUT(pxResults)
    END
  END
  
  ! More parts in search query? Call recursive for each current result. Only when there were results at this level
  IF PosEnd + 1 < LEN(pxQuery) AND RECORDS(pxResults) > 0 THEN
    PosBegin = PosEnd + 1
    Count = 0
    Results &= NEW xResultObjects
    !SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' Next: ''' & SUB(pxQuery, PosBegin, LEN(pxQuery) - PosEnd + 1) & '''')
    LOOP I = 1 TO RECORDS(pxResults)
      GET(pxResults, I)
      !SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' Next Level ' & pxResults.SearchLevel & ' ?= ' & pSearchLevel)
      IF pxResults.SearchLevel <> pSearchLevel THEN CYCLE END
      !SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' Next: ''' & SUB(pxQuery, PosBegin, LEN(pxQuery) - PosEnd + 1) & '''')
      Count += SELF.Search(Results, pxResults.Obj, SUB(pxQuery, PosBegin, LEN(pxQuery) - PosEnd + 1), pSearchLevel + 1, pxResults.Index, pxResults.Count)
      SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' Returned: ''' & SUB(pxQuery, PosBegin, LEN(pxQuery) - PosEnd + 1) & ''', Records=' & RECORDS(Results) & ', Count=' & Count)
    END ! LOOP
    !DISPOSE(SELF.xResults)
    !SELF.xResults &= Results
    ! Return higher level results
    FREE(pxResults)
    LOOP I = 1 TO RECORDS(Results)
      GET(Results, I)
      pxResults = Results
      ADD(pxResults)
    END
    DISPOSE(Results)
    SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' Finished: ''' & SUB(pxQuery, PosBegin, LEN(pxQuery) - PosEnd + 1) & ''', Records=' & RECORDS(pxResults) & ', Count=' & Count)
  END
  SELF.DebugOutput(ALL(' ', pSearchLevel * 2) & 'Search Level=' & pSearchLevel & ' End: ''' & pxQuery & ''', Count=' & Count)
    
  RETURN Count
  

!!! Finds a matching name in any node starting at the current root and adds it to the current result set, optionally specifying in which part of the node the name is matched
xPath.FindAllNodes  PROCEDURE(STRING pNode, BYTE pPathSearch=xPathSearch:Element)!, LONG, VIRTUAL ! Searches all Nodes first to last
  CODE
  RETURN SELF.FindAllNodes(SELF.xResults, SELF.xRoot, pNode, pPathSearch)


!!! Finds a matching name in any node starting at the specified root and adds it to the current result set, optionally specifying in which part of the node the name is matched
xPath.FindAllNodes  PROCEDURE(xObject pxRoot, STRING pNode, BYTE pPathSearch=xPathSearch:Element)!, LONG, VIRTUAL ! Searches all Nodes first to last
  CODE
  RETURN SELF.FindAllNodes(SELF.xResults, pxRoot, pNode, pPathSearch)

  
!!! Finds a matching name in any node starting at the specified root and adds it to the specified result set, optionally specifying in which part of the node the name is matched
xPath.FindAllNodes  PROCEDURE(xResultObjects pxResults, <xObject pxRoot>, STRING pNode, BYTE pPathSearch=xPathSearch:Element)!, LONG, VIRTUAL ! Searches all Nodes first to last
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
  !SELF.DebugOutput('FindAllNodes Begin: ''' & pNode & ''' at ' & pPathSearch & ' in ' & Obj.ToString())
  LOOP
    Count += SELF.MatchNode(pxResults, Obj, pNode, pPathSearch)
    Obj &= Obj.xNext
  UNTIL Obj &= NULL
  !SELF.DebugOutput('FindAllNodes End')
  RETURN Count
  
    
!!! Finds a matching name in any (recursive) child node starting at the current root and adds it to the current result set, optionally specifying in which part of the node the name is matched
xPath.FindAllChildNodes PROCEDURE(STRING pNode, BYTE pPathSearch=xPathSearch:Element, BYTE pRecursive=True)!, LONG, VIRTUAL ! Searches Node and child Nodes
  CODE
  RETURN SELF.FindAllChildNodes(SELF.xResults, SELF.xRoot, pNode, pPathSearch, pRecursive)


!!! Finds a matching name in any (recursive) child node starting at the specified root and adds it to the current result set, optionally specifying in which part of the node the name is matched
xPath.FindAllChildNodes PROCEDURE(xObject pxRoot, STRING pNode, BYTE pPathSearch=xPathSearch:Element, BYTE pRecursive=True)!, LONG, VIRTUAL ! Searches Node and child Nodes
  CODE
  RETURN SELF.FindAllChildNodes(SELF.xResults, pxRoot, pNode, pPathSearch, pRecursive)
  
  
!!! Finds a matching name in any (recursive) child node starting at the specified root and adds it to the specified result set, optionally specifying in which part of the node the name is matched
xPath.FindAllChildNodes PROCEDURE(xResultObjects pxResults, <xObject pxRoot>, STRING pNode, BYTE pPathSearch=xPathSearch:Element, BYTE pRecursive=True)!, LONG, VIRTUAL ! Searches Node and child Nodes
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
  !SELF.DebugOutput(ALL(' ', pRecursive * 2) & 'FindAllChildNodes Begin: ''' & pNode & ''' at ' & pPathSearch & ' in ' & Obj.ToString())
  Count += SELF.MatchNode(pxResults, Obj, pNode, BAND(pPathSearch, xPathSearch:Attribute))
  !SELF.DebugOutput(ALL(' ', pRecursive * 2) & 'FindAllChildNodes Recursive=' & pRecursive & ', Childs=' & RECORDS(Obj.xChilderen))
  IF NOT Obj.xChilderen &= NULL AND RECORDS(Obj.xChilderen) > 0 THEN
    LOOP I = 1 TO RECORDS(Obj.xChilderen)
      GET(Obj.xChilderen, I)
      Count += SELF.MatchNode(pxResults, Obj.xChilderen.Obj, pNode, BAND(pPathSearch, xPathSearch:Element))
      IF pRecursive > 0 THEN
        Count += SELF.FindAllChildNodes(pxResults, Obj.xChilderen.Obj, pNode, pPathSearch, pRecursive + 1)
      END
    END
  END
  !SELF.DebugOutput('FindAllChildNodes End')
  RETURN Count

  
!!! Compares a node with a name and adds it to the current result set, optionally specifying in which part of the node the name is matched
xPath.MatchNode     PROCEDURE(xObject pxObj, STRING pNode, BYTE pPathSearch=xPathSearch:Element)!, LONG, PRIVATE, VIRTUAL ! Searches Node
  CODE
  RETURN SELF.MatchNode(SELF.xResults, pxObj, pNode, pPathSearch)
  
  
!!! Compares a node with a name and adds it to the specified result set, optionally specifying in which part of the node the name is matched
xPath.MatchNode     PROCEDURE(xResultObjects pxResults, xObject pxObj, STRING pNode, BYTE pPathSearch=xPathSearch:Element)!, LONG, PRIVATE, VIRTUAL ! Searches Node
I                     LONG
Count                 LONG

  CODE
  IF pxObj &= NULL THEN RETURN 0 END

  IF SUB(pNode, 1, 1) = xPathAttribute THEN ! @  : Selects attributes
    pPathSearch = BAND(pPathSearch, xPathSearch:Attribute)
    IF SUB(pNode, 2, 1) <> xPathWildcard THEN
      pNode = SUB(pNode, 2, LEN(pNode) - 1)
    END
  END

  IF pPathSearch = xPathSearch:None THEN RETURN 0 END
  !SELF.DebugOutput('MatchNode Begin ' & pNode & ' at ' & pPathSearch & '=' & CHOOSE(BAND(pPathSearch, xPathSearch:Element) > 0 , 'node', '') & CHOOSE(BAND(pPathSearch, xPathSearch:Attribute) > 0 , 'attribute', '') & ' in ' & pxObj.ToString())
  ! Test match with tag
  IF BAND(pPathSearch, xPathSearch:Element) = xPathSearch:Element THEN
    IF pxObj.LengthTag() > 0 AND pxObj.IsClose() = False AND |
        (UPPER(pxObj.Tag) = UPPER(pNode) OR UPPER(pxObj.Tag) = UPPER(xPathAnyNode) OR pxObj.Tag = xPathAnyElement) THEN
      SELF.DebugOutput('MatchNode found Node: ' & pNode & ' = ' & pxObj.ToString())
      SELF.AddNode(pxResults, pxObj, 0)
      Count += 1
    END
  END
  ! Test match with attribute
  IF pxObj.CountAttributes() > 0 AND BAND(pPathSearch, xPathSearch:Attribute) = xPathSearch:Attribute THEN 
    IF UPPER(pNode) = UPPER(xPathAnyNode) OR pNode = xPathAnyAttribute THEN
      LOOP I = 1 TO pxObj.CountAttributes()
        SELF.DebugOutput('MatchNode found attribute ' & pNode & ' = [' & I & ']: ' & pxObj.ToString())
        SELF.AddNode(pxResults, pxObj, I)
        Count += 1
      END ! LOOP
    ELSE
      I = pxObj.FindAttribute(pNode)
      IF I > 0 THEN
        SELF.DebugOutput('MatchNode found attribute ' & pNode & ' = [' & I & ']: ' & pxObj.ToString())
        SELF.AddNode(pxResults, pxObj, I)
        Count += 1
      END ! IF I > 0
    END
  END
  !SELF.DebugOutput('MatchNode End')
  RETURN Count
  
  
!!! Adds a node or attribute to the current result set
xPath.AddNode       PROCEDURE(xObject pxObj, LONG pIndex)!, PRIVATE, VIRTUAL ! Adds a Node to search results
  CODE
  SELF.AddNode(SELF.xResults, pxObj, pIndex)
  
  
!!! Adds a node or attribute to the supplied result set
xPath.AddNode       PROCEDURE(xResultObjects pxResults, xObject pxObj, LONG pIndex)!, PRIVATE, VIRTUAL ! Adds a Node to search results
  CODE
  IF pxResults &= NULL THEN RETURN END
  IF pxObj     &= NULL THEN RETURN END
  pxResults.Obj            &= pxObj
  pxResults.AttributeIndex  = pIndex
  pxResults.SearchLevel     = xPathSearchLevel:Unknown
  !SELF.DebugOutput('AddNode ' & pIndex & ': ' & pxObj.ToString())
  ADD(pxResults)

  
!!! Returns all nodes in the reult set, tab seperated with path and value/contents
xPath.ToString      PROCEDURE()!, STRING, VIRTUAL
I                             LONG
X                             gcCString
  CODE
  IF SELF.xResults &= NULL THEN RETURN '' END
  IF RECORDS(SELF.xResults) = 0 THEN RETURN '' END
  X.Init(1024 * 1024)
  LOOP I = 1 TO RECORDS(SELF.xResults)
    GET(SELF.xResults, I)
    X.Value = X.Value & SELF.xResults.Obj.ToStringPath(xPathSeperator)
    IF SELF.xResults.AttributeIndex = 0 THEN
      X.Value = X.Value & xPathSeperator & SELF.xResults.Obj.GetTag() & '<9>' & SELF.xResults.Obj.ToStringContents(False) & '<13,10>'
    ELSE
      X.Value = X.Value & xPathSeperator & xPathAttribute & SELF.xResults.Obj.GetAttributeLabel(SELF.xResults.AttributeIndex) & '<9>' & | 
                                                            SELF.xResults.Obj.GetAttributeValue(SELF.xResults.AttributeIndex) & '<13,10>'
    END
  END
  RETURN X.Value

  
!!! Returns all nodes in the reult set
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

  
!!! Returns number of records in the result set, this can be used to loop with GetResult(x)
xPath.Records       PROCEDURE()!, LONG, VIRTUAL
  CODE
  IF SELF.xResults &= NULL THEN RETURN 0 END
  RETURN RECORDS(SELF.xResults)
  
  
!!! Returns a single item in the result set
xPath.GetResult     PROCEDURE(LONG pIndex)!, *xObject, VIRTUAL
  CODE
  IF SELF.xResults &= NULL THEN RETURN NULL END
  GET(SELF.xResults, pIndex)
  IF ERRORCODE() THEN RETURN NULL END
  RETURN SELF.xResults.Obj

  
! Debug

        
!!! Debugging
xPath.DebugOutput  PROCEDURE(STRING pMessage)!, VIRTUAL
Msg CSTRING(1024)
  CODE
  Msg = 'xPath: ' & pMessage ! & '<13,10>' & SELF.ToString()
  OutputDebugString(Msg)


