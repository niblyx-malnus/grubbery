|%
++  engine
  |*  [nod=mold med=mold fin=mold]
  |%
  +$  vis  (map nod med)
  ++  form
    $_  ^|
    |%
    ++  flow  |~(nod *(list nod))
    ++  init  |~(nod *med)
    ++  stop  |~([nod med] `?`%.n)
    ++  meld  |~([nod neb=nod old=med new=med] new)
    ++  land  |~([nod =med ?] med)
    ++  exit  |~([nod vis=(map nod med)] `fin`!!)
    ++  prnt  |~(nod *tape)
    --
  :: traverse the underlying DAG (directed acyclic graph)
  ::
  ++  traverse
    :: takes a form core
    :: and a map of already visited nodes to values
    :: 
    |=  [form =vis]
    :: initialize path to catch cycles
    ::
    =|  pat=(list nod)
    :: accept single node gid
    ::
    |=  =nod
    :: final transformation
    ::
    ^-  fin
    %+  exit  nod
    |-
    ^-  (map ^nod med)
    :: catch cycles
    ::
    =/  i  (find [nod]~ pat) 
    =/  cyc=(list ^nod)  ?~(i ~ [nod (scag +(u.i) pat)])
    :: TODO: never allow prnt to crash
    ?^  cyc
      ~|(["cycle" (turn cyc prnt)] !!)
    :: iterate through neighbors (flo)
    ::
    =/  flo=(list ^nod)  (flow nod)
    :: initialize output
    ::
    =/  =med  (init nod)
    |-
    :: stop when neighbors exhausted or stop condition met
    :: 
    =/  cnd=?  (stop nod med)
    ?:  |(cnd ?=(~ flo))
      :: output according to land function
      ::
      (~(put by vis) nod (land nod med cnd))
    :: make sure visited reflects output of next neighbor
    ::
    =.  vis
      ?:  (~(has by vis) i.flo)
        vis :: if already in visited, use stored output
      :: recursively compute output for next neighbor
      ::
      %=  ^$
        nod  i.flo
        pat  [nod pat]
        vis  vis :: this has been updated by inner trap
      ==
    :: update the output and go to the next neighbor
    ::
    %=  $
      flo  t.flo
      :: meld the running output with the new output
      ::
      med  (meld nod i.flo med (~(got by vis) i.flo))
    ==
  ::
  ++  chain
    |=  [nodes=(list nod) =vis]
    ^+  vis
    ?~  nodes
      vis
    %=  $
      nodes  t.nodes
      vis    (traverse i.nodes vis)
    ==
  --
--
