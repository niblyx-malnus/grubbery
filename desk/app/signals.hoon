/-  s=signals
/+  signals, dbug, verb, default-agent
/=  x-  /ted/test
|%
+$  state-0  [%0 =land:s =trac:s]
+$  card     card:agent:gall
--
::
=|  state-0
=*  state  -
::
=<
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  [~ this]
::
++  on-save   !>(state)
::
++  on-load
  |=  ole=vase
  ^-  (quip card _this)
  [~ this]
::
++  on-peek   on-peek:def
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ~&  "poke to {<dap.bowl>} agent with mark {<mark>}"
  ?+    mark  (on-poke:def mark vase)
      %make-root
    =+  !<([here=path stud=path data=^vase] vase)
    ~&  here+here
    =.  land  (~(put of land) here [data stud [%root ~]])
    =.  trac
      %+  ~(put of trac)  here
      ?~  tac=(~(get of trac) here)
        [now.bowl ~ ~ ~]
      u.tac(last (new-last now.bowl last.u.tac))
    =^  cards  this
      (on-poke poke-root+!>([here /init !>(~)]))
    ::
    =^  cadz  state
      (dirty-and-tidy:hc here)
    [(welp cards cadz) this]
    ::
      %make-stem
    =+  !<([here=path stud=path sour=(set path)] vase)
    ~&  here+here
    ?<  ?=([%fil *] here) :: stems not allowed in /fil
    =/  =node:s  [!>(~) stud [%stem [| ~] ~]]
    =.  land     (~(put of land) here node)
    =.  state    (add-sources here sour)
    =.  trac
      %+  ~(put of trac)  here
      ?~  tac=(~(get of trac) here)
        [now.bowl ~ ~ ~]
      u.tac(last (new-last now.bowl last.u.tac))
    =^  cards  state
      (dirty-and-tidy:hc here)
    [cards this]
    ::
      %oust-node
    =+  !<(here=path vase)
    ~&  here+here
    =/  =node:s  (need (~(get of land) here))
    =^  cards  this
      ?.  ?=(%root -.kind.node)
        [~ this]
      (on-poke poke-root+!>([here /kill !>(~)]))
    =.  land  (~(del of land) here)
    =/  =tack:s  (need (~(get of trac) here))
    [~ this(trac (~(put of trac) here tack(last (new-last now.bowl last.tack))))]
    ::
      %poke-root
    =+  !<([here=path =pail:s] vase)
    ~&  here+here
    =/  =node:s  (need (~(get of land) here))
    ?>  ?=(%root -.kind.node)
    =/  =bowl:root:s  [now our src eny here (~(dip of land) here)]:[bowl .]
    =/  res=(each [cards=(list card) data=^vase] tang)
      %-  mule  |.
      =/  =root:s  (get-root:hc here)
      (root bowl data.node pail)
    ?:  ?=(%| -.res)
      (mean p.res)
    =?  trac  !=(data.node data.p.res)
      =/  =tack:s  (need (~(get of trac) here))
      (~(put of trac) here tack(last (new-last now.bowl last.tack)))
    =.  land  (~(put of land) here node(data data.p.res))
    =^  tidy-cards  state
      (dirty-and-tidy:hc here)
    [(welp cards.p.res tidy-cards) this] :: stem cards come after root
    ::
      %make-root-2
    =+  !<([here=path stud=path data=^vase] vase)
    ~&  here+here
    =^  cards  state
      (make-root:hc here stud data)
    [cards this]
    ::
      %make-stem-2
    =+  !<([here=path stud=path sour=(set path)] vase)
    ~&  here+here
    =^  cards  state
      (make-stem:hc here stud sour)
    [cards this]
    ::
      %poke-root-2
    =+  !<([=wire here=path =pail:s] vase)
    ~&  here+here
    =^  cards  state
      (poke-root:hc wire here pail)
    [cards this]
    ::
      %bump-root-2
    =+  !<([=wire here=path =pail:s] vase)
    ~&  here+here
    =^  cards  state
      (bump-root:hc wire here pail)
    [cards this]
    ::
      %kill-root-2
    =+  !<(here=path vase)
    ~&  here+here
    =^  cards  state
      (kill-root:hc here)
    [cards this]
    ::
      %tidy-stem
    =+  !<(here=path vase)
    ~&  here+here
    =^  cards  state
      (tidy-stem:hc here)
    [cards this]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
      [%vent @ta *]
    ?>  =(src.bowl (slav %p i.t.path))
    `this
  ==
::
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
::
=|  gall-cards=(list card:agent:gall)
|_  =bowl:gall
+*  this  .
++  abet  [(flop gall-cards) state]
++  emit  |=(=card:agent:gall this(gall-cards [card gall-cards]))
++  emil
  |=  cadz=(list card:agent:gall)
  this(gall-cards (welp cadz gall-cards))
++  new-last
  |=  [now=@da last=@da]
  ^-  @da
  ?:  (lth last now)
    now
  +(last)
::
++  get-root
  |=  here=path
  ^-  root:s
  ?<  ?=([%bin *] here) :: all /bin nodes are roots
  ?:  ?=([%fil *] here)  root:fil:signals :: /fil nodes
  =/  =node:s  (need (~(get of land) (welp /bin/nod here)))
  ?>  ?=(%stem -.kind.node)
  ?>  =([%& ~] tidy.kind.node)
  !<(root:s data.node)
::
++  get-root-2
  |=  here=path
  ^-  root:proc:s
  ?<  ?=([%bin *] here) :: all /bin nodes are roots
  ?:  ?=([%fil *] here)  root-2:fil:signals :: /fil nodes
  =/  =node:s  (need (~(get of land) (welp /bin/nod here)))
  ?>  ?=(%stem -.kind.node)
  ?>  =([%& ~] tidy.kind.node)
  !<(root:proc:s data.node)
::
++  get-stem
  |=  here=path
  ^-  stem:s
  ?<  ?=([%fil *] here) :: all /fil nodes are roots
  ?:  ?=([%bin *] here)  stem:bin:signals :: /bin nodes
  =/  =node:s  (need (~(get of land) (welp /bin/nod here)))
  ?>  ?=(%stem -.kind.node)
  ?>  =([%& ~] tidy.kind.node)
  !<(stem:s data.node)
::
++  get-stem-2
  |=  here=path
  ^-  stem-2:s
  ?<  ?=([%fil *] here) :: all /fil nodes are roots
  ?:  ?=([%bin *] here)  stem-2:bin:signals :: /bin nodes
  =/  =node:s  (need (~(get of land) (welp /bin/nod here)))
  ?>  ?=(%stem -.kind.node)
  ?>  =([%& ~] tidy.kind.node)
  !<(stem-2:s data.node)
::
++  no-cycle
  =|  hist=(list path)
  |=  here=path
  ^-  ?
  =/  i=(unit @ud)  (find [here]~ hist) 
  =/  cycle=(list path)  ?~(i ~ [here (scag +(u.i) hist)])
  ?^  cycle
    ~&  ["ERROR: cycle" cycle]
    %| :: a cycle has been found
  ?~  nod=(~(get of land) here)
    %& :: non-existent nodes aren't a cycle
  ?:  ?=(?(%root %root-2) -.kind.u.nod)
    %&
  :: TODO: remove unnecessary assertion
  ?>  ?=(%stem -.kind.u.nod)
  =/  sour=(list path)  ~(tap in ~(key by sour.kind.u.nod))
  |-
  ?~  sour
    %&
  ?.  ^$(hist [here hist], here i.sour)
    %|
  $(sour t.sour)
::
++  no-cycles
  |=  [here=path sour=(list path)]
  ^-  ?
  ?~  sour
    %&
  ?.  %*($ no-cycle hist ~[here], here i.sour)
    %|
  $(sour t.sour)
::
++  add-sources
  |=  [here=path sour=(set path)]
  ^+  state
  =/  sour=(list path)  ~(tap in sour)
  ?>  (no-cycles here sour)
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%stem -.kind.node)
  |-
  ?~  sour
    state(land (~(put of land) here node))
  ?>  (~(has of land) i.sour)
  =/  =tack:s  (need (~(get of trac) i.sour))
  =.  sinx.tack  (~(put in sinx.tack) here)
  =.  trac  (~(put of trac) i.sour tack)
  =.  sour.kind.node  (~(put by sour.kind.node) i.sour last.tack)
  $(sour t.sour)
::
++  del-sources
  |=  [here=path sour=(set path)]
  ^+  state
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%stem -.kind.node)
  =/  sour=(list path)  ~(tap in sour)
  =.  sour.kind.node
    |-
    ?~  sour
      sour.kind.node
    %=  $
      sour            t.sour
      sour.kind.node  (~(del by sour.kind.node) i.sour)
    ==
  |-
  ?~  sour
    state
  =/  =tack:s  (need (~(get of trac) i.sour))
  =.  sinx.tack  (~(del in sinx.tack) here)
  $(sour t.sour, trac (~(put of trac) i.sour tack))
::
++  dirty
  |=  here=path
  ^-  [(set path) land:s]
  =/  =node:s  (need (~(get of land) here))
  =?  land  ?=(%stem -.kind.node)
    %+  ~(put of land)  here
    node(kind [%stem [| ~] sour]:kind.node)
  =/  =tack:s  (need (~(get of trac) here))
  ?:  =(0 ~(wyt in sinx.tack))
    [(sy ~[here]) land]
  =/  sinx=(list path)  ~(tap in sinx.tack)
  =|  edge=(set path)
  |-
  ?~  sinx
    [edge land]
  =^  e  land
    (dirty i.sinx)
  $(sinx t.sinx, edge (~(uni in edge) e))
::
++  make-deps
  |=  sour=(set path)
  ^-  (map path vase)
  %-  ~(gas by *(map path vase))
  %+  turn  ~(tap in sour)
  |=  =path
  :-  path
  data:(need (~(get of land) path))
::
++  tidy-stem
  |=  here=path
  ^-  (quip card _state)
  ?~  nod=(~(get of land) here)
    :: deal with non-existent source later
    ::
    [~ state]
  =/  =node:s  u.nod
  ?.  ?=(%stem -.kind.node)
    :: roots are always tidy
    ::
    [~ state]
  ?:  flag.tidy.kind.node
    :: tidy stems stay tidy
    ::
    [~ state]
  =/  sour=(list (pair path @da))  ~(tap by sour.kind.node)
  :: tidy each source
  ::
  =^  cards  state
    =|  cards=(list card)
    |-
    ?~  sour
      [cards state]
    =^  cadz  state
      (tidy-stem p.i.sour)
    $(sour t.sour, cards (welp cadz cards))
  :: check sources to see if we need to tidy ourselves
  ::
  :: if we have no sources we are being asked to recompute
  ::
  =/  tidy=?  =(~ sour)
  |-
  ?:  tidy
    ?~  sour
      :: we've checked all sources and don't need to recompute
      ::
      =.  node  node(tidy.kind [%& ~])
      [cards state(land (~(put of land) here node))]
    ?~  dep=(~(get of land) p.i.sour)
      :: If the dep doesn't exist, update boom
      ::
      =/  =tang
        :~  leaf+"stem boom"
            leaf+(spud here)
            leaf+"{(spud p.i.sour)} has no node"
        ==
      %-  (slog tang)
      =.  boom.tidy.kind.node  [~ tang]
      [cards state(land (~(put of land) here node))]
    ?:  &(?=(%stem -.kind.u.dep) ?=(^ boom.tidy.kind.u.dep))
      :: If a source is a stem and has a boom (crashed on recompute),
      :: we inherit its boom
      ::
      =.  boom.tidy.kind.node  boom.tidy.kind.u.dep
      [cards state(land (~(put of land) here node))]
    :: if the source last has changed, we are not tidy
    ::
    ?:  (lth q.i.sour last:(need (~(get of trac) p.i.sour)))
      $(tidy %|)
    $(sour t.sour)
  :: If we are not tidy because our sources have updated
  :: recompute here
  ::
  =/  res=(each [cards=(list card) data=vase] tang)
    %-  mule  |.
    =/  =stem:s  (get-stem here)
    =/  deps=(map path vase)  (make-deps ~(key by sour.kind.node))
    (stem [now our eny here deps]:[bowl .])
  ?-    -.res
      %|
    :-  cards
    =/  =tang  [leaf+"stem boom" leaf+(spud here) p.res]
    %-  (slog tang)
    state(land (~(put of land) here node(tidy.kind [%| ~ tang])))
      %&
    :-  (welp cards cards.p.res) :: source cards go first
    =?  trac  !=(data.node data.p.res)
      =/  =tack:s  (need (~(get of trac) here))
      (~(put of trac) here tack(last (new-last now.bowl last.tack)))
    =.  node  node(data data.p.res, tidy.kind [%& ~])
    state(land (~(put of land) here node))
  ==
::
++  dirty-and-tidy
  |=  here=path
  ^-  (quip card _state)
  =^  e  land
    (dirty here)
  =/  edge=(list path)  ~(tap in e)
  =|  cards=(list card)
  |-
  ?~  edge
    [cards state]
  =^  cadz  state
    (tidy-stem i.edge)
  %=  $
    edge   t.edge
    cards  (welp cards cadz)
  ==
::
++  dirty-and-tidy-2
  |=  here=path
  ^-  (quip card-2:s _state)
  =^  e  land
    (dirty here)
  =/  edge=(list path)  ~(tap in e)
  =|  cards=(list card-2:s)
  |-
  ?~  edge
    [cards state]
  =^  cadz  state
    (tidy-stem-2 i.edge)
  %=  $
    edge   t.edge
    cards  (welp cards cadz)
  ==
::
++  tidy-stem-2
  |=  here=path
  ^-  (quip card-2:s _state)
  ?~  nod=(~(get of land) here)
    :: deal with non-existent source later
    ::
    [~ state]
  =/  =node:s  u.nod
  ?.  ?=(%stem -.kind.node)
    :: roots are always tidy
    ::
    [~ state]
  ?:  flag.tidy.kind.node
    :: tidy stems stay tidy
    ::
    [~ state]
  =/  sour=(list (pair path @da))  ~(tap by sour.kind.node)
  :: tidy each source
  ::
  =^  cards  state
    =|  cards=(list card-2:s)
    |-
    ?~  sour
      [cards state]
    =^  cadz  state
      (tidy-stem-2 p.i.sour)
    $(sour t.sour, cards (welp cadz cards))
  :: check sources to see if we need to tidy ourselves
  ::
  :: if we have no sources we are being asked to recompute
  ::
  =/  tidy=?  =(~ sour)
  |-
  ?:  tidy
    ?~  sour
      :: we've checked all sources and don't need to recompute
      ::
      =.  node  node(tidy.kind [%& ~])
      [cards state(land (~(put of land) here node))]
    ?~  dep=(~(get of land) p.i.sour)
      :: If the dep doesn't exist, update boom
      ::
      =/  =tang
        :~  leaf+"stem boom"
            leaf+(spud here)
            leaf+"{(spud p.i.sour)} has no node"
        ==
      %-  (slog tang)
      =.  boom.tidy.kind.node  [~ tang]
      [cards state(land (~(put of land) here node))]
    ?:  &(?=(%stem -.kind.u.dep) ?=(^ boom.tidy.kind.u.dep))
      :: If a source is a stem and has a boom (crashed on recompute),
      :: we inherit its boom
      ::
      =.  boom.tidy.kind.node  boom.tidy.kind.u.dep
      [cards state(land (~(put of land) here node))]
    :: if the source last has changed, we are not tidy
    ::
    ?:  (lth q.i.sour last:(need (~(get of trac) p.i.sour)))
      $(tidy %|)
    $(sour t.sour)
  :: If we are not tidy because our sources have updated
  :: recompute here
  ::
  =/  res=(each [cards=(list card-2:s) data=vase] tang)
    %-  mule  |.
    =/  =stem-2:s  (get-stem-2 here)
    =/  deps=(map path vase)  (make-deps ~(key by sour.kind.node))
    (stem-2 [now our eny here deps]:[bowl .])
  ?-    -.res
      %|
    :-  cards
    =/  =tang  [leaf+"stem boom" leaf+(spud here) p.res]
    %-  (slog tang)
    state(land (~(put of land) here node(tidy.kind [%| ~ tang])))
      %&
    :-  (welp cards cards.p.res) :: source cards go first
    =?  trac  !=(data.node data.p.res)
      =/  =tack:s  (need (~(get of trac) here))
      (~(put of trac) here tack(last (new-last now.bowl last.tack)))
    =.  node  node(data data.p.res, tidy.kind [%& ~])
    state(land (~(put of land) here node))
  ==
::
++  make-root
  |=  [here=path stud=path data=vase]
  ^-  (quip card _state)
  =/  =node:s  [data stud [%root-2 ~]]
  =.  land  (~(put of land) here node)
  =.  trac
    %+  ~(put of trac)  here
    ?~  tac=(~(get of trac) here)
      [now.bowl ~ ~ ~]
    u.tac(last (new-last now.bowl last.u.tac))
  ~&  >>  %here
  (poke-root / here /init !>(~))
::
++  make-stem
  |=  [here=path stud=path sour=(set path)]
  ^-  (quip card _state)
  ?<  ?=([%fil *] here) :: stems not allowed in /fil
  =/  =node:s  [!>(~) stud [%stem [| ~] ~]]
  =.  land     (~(put of land) here node)
  =.  state    (add-sources here sour)
  =.  trac
    %+  ~(put of trac)  here
    ?~  tac=(~(get of trac) here)
      [now.bowl ~ ~ ~]
    u.tac(last (new-last now.bowl last.u.tac))
  =^  cards  state
    (dirty-and-tidy-2 here)
  (apply-cards here cards)
::
++  poke-root
  |=  [=wire here=path =pail:s]
  ^-  (quip card _state)
  ~&  %poke-root
  =/  from=path  [(scot %p our.bowl) sap.bowl]
  =^  cards  state
    (take-poke here [from wire] pail)
  (apply-cards here cards)
::
++  bump-root
  |=  [=wire here=path =pail:s]
  ^-  (quip card _state)
  =/  from=path  [(scot %p our.bowl) sap.bowl]
  =^  cards  state
    (take-bump here [from wire] pail)
  (apply-cards here cards)
::
++  kill-root
  |=  here=path
  ^-  (quip card _state)
  =^  cards  state
    (take-kill here)
  (apply-cards here cards)
::
++  apply-cards
  =|  sysc=(list card)
  |=  [here=path cards=(list card-2:s)]
  ^-  (quip card _state)
  ~&  %apply-cards
  =/  from=path  (weld /(scot %p our.bowl)/gall/signals here)
  ?~  cards
    [(flop sysc) state]
  ?-    -.i.cards
      %sysc
    $(cards t.cards, sysc [card.i.cards sysc])
    ::
      %poke
    =^  cadz  state
      (take-poke path [from wire] pail):[i.cards .]
    $(cards (welp cadz t.cards))
    ::
      %bump
    =^  cadz  state
      (take-bump path [from wire] pail):[i.cards .]
    $(cards (welp cadz t.cards))
    ::
      %peek
    =^  cadz  state
      (take-peek here [wire path]:i.cards)
    $(cards (welp cadz t.cards))
  ==
::
++  take-peek
  |=  [here=path =wire =path]
  ^-  (quip card-2:s _state)
  (ingest here ~ %peek wire path (~(dip of land) path))
::
++  take-kill
  |=  here=path
  ^-  (quip card-2:s _state)
  =/  =tack:s  (need (~(get of trac) here))
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%root-2 -.kind.node)
  =.  land  (~(put of land) here node(proc.kind ~))
  =|  cards=(list card-2:s)
  |-
  =^  cadz  state
    (give-poke-result here %| %killed [leaf+(spud here) ~])
  =^  poke  trac
    (stage-next-poke here)
  ?~  poke 
    [cards state]
  $(cards (welp cards cadz))
::
++  take-bump
  |=  [here=path =give:s =pail:s]
  ^-  (quip card-2:s _state)
  =/  res=(each [cards=(list card-2:s) =_state] tang)
    (mule |.((ingest here ~ %bump pail)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =^  cards  state
    ?-  -.res
      %&  p.res
      %|  [~ state]
    ==
  =^  cadz  state
    ?.  ?=([@t %gall %signals *] from.give)
      :_  state
      :~  [%sysc %give %fact ~[wire.give] sign-root+!>([%bump err])]
          [%sysc %give %kick ~[wire.give] ~]
      ==
    %+  ingest
      t.t.t.from.give
    [~ %root wire.give %bump err]
  [(welp cadz cards) state]
::
++  take-poke
  |=  [here=path =poke:s]
  ^-  (quip card-2:s _state)
  ~&  %take-poke
  =/  =tack:s  (need (~(get of trac) here))
  =.  trac  (~(put of trac) here tack(line (~(put to line.tack) poke)))
  (run-next-poke here)
::
++  stage-next-poke
  |=  here=path
  ^-  [(unit poke:s) trac:s]
  =/  =tack:s  (need (~(get of trac) here))
  ?>  ?=(~ give.tack)
  ?:  =(~ line.tack)
    [~ trac] :: no more pokes to run
  =/  [=poke:s line=(qeu poke:s)]  ~(get to line.tack)
  [`poke (~(put of trac) here tack(give `give.poke, line line))]
::
++  run-next-poke
  |=  here=path
  ^-  (quip card-2:s _state)
  ~&  %run-next-poke
  =/  =node:s  (need (~(get of land) here))
  =^  poke  trac
    (stage-next-poke here)
  ~&  poke+poke
  ?~  poke 
    [~ state]
  ?>  ?=(%root-2 -.kind.node)
  =/  =bowl:proc:s  [now our src eny sap here]:[bowl .]
  =/  build=(each proc:proc:s tang)
    (mule |.(((get-root-2 here) bowl pail.u.poke)))
  ?:  ?=(%& -.build)
    =.  land  (~(put of land) here node(proc.kind `p.build))
    (ingest here ~) :: start
  %-  (slog [leaf+"build-fail" p.build])
  =^  cards  state
    (give-poke-result here %| %build-fail p.build)
  =^  cadz  state
    (run-next-poke here)
  [(welp cards cadz) state]
::
++  give-poke-result
  |=  [here=path res=(each pail:s goof)]
  ^-  (quip card-2:s _state)
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%root-2 -.kind.node)
  =/  =tack:s  (need (~(get of trac) here))
  ?>  ?=(^ give.tack)
  =^  cards  state
    ?.  ?=([@t %gall %signals *] from.u.give.tack)
      :_  state
      :~  [%sysc %give %fact ~[wire.u.give.tack] sign-root+!>([%poke res])]
          [%sysc %give %kick ~[wire.u.give.tack] ~]
      ==
    %+  ingest
      t.t.t.from.u.give.tack
    [~ %root wire.u.give.tack %poke res]
  =.  land  (~(put of land) here node(proc.kind ~))
  =.  trac  (~(put of trac) here tack(give ~))
  [cards state]
::
++  ingest
  |=  [here=path in=(unit intake:proc:s)]
  ^-  (quip card-2:s _state)
  ~&  >>  %ingest
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%root-2 -.kind.node)
  ?>  ?=(^ proc.kind.node)
  =/  =bowl:proc:s  [now our src eny sap here]:[bowl .]
  =/  m  (charm:proc:s ,pail:s)
  =/  res=(each [[(list card-2:s) vase result:eval:m] proc:proc:s] tang)
    (mule |.((take:eval:m u.proc.kind.node bowl data.node in)))
  =/  [[cards=(list card-2:s) data=vase =result:eval:m] =proc:proc:s]
    ?-  -.res
      %&  p.res
      %|  [[~ data.node [%fail %crash [leaf+(spud here) p.res]]] u.proc.kind.node]
    ==
  ::
  =/  tick=?  !=(data data.node)
  =?  trac  tick
    =/  =tack:s  (need (~(get of trac) here))
    (~(put of trac) here tack(last (new-last now.bowl last.tack)))
  =.  node  node(data data)
  =.  land
    ?.  ?=(%next -.result)
      (~(put of land) here node(data data, proc.kind ~))
    (~(put of land) here node(data data, proc.kind `proc))
  ::
  =^  tidy-cards  state
    ?.  tick
      [~ state]
    (dirty-and-tidy-2 here)
  =.  cards  (welp cards tidy-cards)
  ::
  ?:  ?=(%next -.result)
    [cards state]
  =^  res-cards  state
    %+  give-poke-result
      here
    ?-  -.result
      %fail  [%| err.result]
      %done  [%& value.result]
    ==
  ::
  =^  nex-cards  state
    (run-next-poke here)
  [:(welp cards res-cards nex-cards) state]
--
