/-  s=signals
/+  signals, dbug, verb, default-agent
/=  x-  /ted/test
/=  x-  /ted/test-2
/=  x-  /mar/sign-root
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
      %make-root
    =+  !<([=wire here=path stud=path data=^vase] vase)
    ~&  here+here
    =^  cards  state
      abet:(make-root:hc wire here stud data)
    [cards this]
    ::
      %make-stem
    =+  !<([here=path stud=path sour=(set path)] vase)
    ~&  here+here
    =^  cards  state
      abet:(make-stem:hc here stud sour)
    ~&  >>>  %making-stem-end
    [cards this]
    ::
      %poke-root
    =+  !<([=wire here=path =pail:s] vase)
    ~&  here+here
    =^  cards  state
      abet:(poke-root:hc wire here pail)
    [cards this]
    ::
      %bump-root
    =+  !<([=wire here=path =pail:s] vase)
    ~&  here+here
    =^  cards  state
      abet:(bump-root:hc wire here pail)
    [cards this]
    ::
      %kill-root
    =+  !<(here=path vase)
    ~&  here+here
    =^  cards  state
      abet:(kill-root:hc here)
    [cards this]
    ::
      %tidy-stem
    =+  !<(here=path vase)
    ~&  here+here
    =^  cards  state
      abet:(tidy-stem:hc here)
    [cards this]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
      [%root @ *]
    =^  cards  state
      abet:(take-watch path)
    [cards this]
    ::
      [%vent @ *]
    ?>  =(src.bowl (slav %p i.t.path))
    [~ this]
  ==
::
++  on-leave
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-leave:def path)
      [%root @ *]
    =^  cards  state
      abet:(take-leave path)
    [cards this]
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ~&  >  %on-agent
  =^  cards  state
    abet:(take-agent:hc wire sign)
  [cards this]
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  =^  cards  state
    abet:(take-arvo:hc wire sign)
  [cards this]
::
++  on-fail   on-fail:def
--
::
=|  cards=(list card)
=|  bolts=(list bolt:s)
|_  =bowl:gall
+*  this  .
++  emit-card   |=(=card this(cards [card cards]))
++  emit-cards  |=(cadz=(list card) this(cards (welp (flop cadz) cards)))
++  emit-bolt   |=(=bolt:s this(bolts [bolt bolts]))
++  emit-bolts  |=(botz=(list bolt:s) this(bolts (welp (flop botz) bolts)))
:: handle all bolts and return effects and state
::
++  abet
  =.  bolts  (flop bolts)
  |-
  ?~  =(~ bolts)
    [(flop cards) state]
  :: TODO: sandboxing system
  =/  [here=path =dart:s]  ?~(bolts !! i.bolts)
  =.  bolts  ?~(bolts !! t.bolts)
  =/  from=path  (weld /(scot %p our.bowl)/gall/[dap.bowl]/$ here)
  ?-    -.dart
      %sysc
    :: TODO: keep track of scrying with %keen so you can
    ::       so you can cancel with %yawn when you kill
    ::       a process or when it crashes
    ::
    $(this (emit-card (wrap-sysc-card here card.dart)))
    ::
      %poke
    $(this (take-poke path [from wire] pail):[dart .])
    ::
      %bump
    $(this (take-bump path [from wire] pail):[dart .])
    ::
      %peek
    $(this (take-peek here [wire path]:dart))
  ==
::
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
  ?:  ?=(?(%root %root) -.kind.u.nod)
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
  ?.  (~(has of land) i.sour)
    ~|(source+i.sour !!)
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
  ^+  this
  ~&  >>  %tidy-stem
  :: deal with non-existent source later
  ::
  ?~  nod=(~(get of land) here)  this
  =/  =node:s  u.nod
  :: roots are always tidy
  ::
  ?.  ?=(%stem -.kind.node)  this
  :: tidy stems stay tidy
  ::
  ?:  flag.tidy.kind.node  this
  =/  sour=(list (pair path @da))  ~(tap by sour.kind.node)
  :: tidy each source
  ::
  =.  this
    |-
    ?~  sour
      this
    =.  this  (tidy-stem p.i.sour)
    $(sour t.sour)
  :: check sources to see if we need to tidy ourselves;
  :: if we have no sources we are being asked to recompute
  ::
  =/  tidy=?  =(~ sour)
  |-
  ?:  tidy
    ?~  sour
      :: we've checked all sources and don't need to recompute
      ::
      =.  node  node(tidy.kind [%& ~])
      =.  land  (~(put of land) here node)
      this
    ?~  dep=(~(get of land) p.i.sour)
      :: If the dep doesn't exist, update boom
      ::
      =/  =tang
        :~  leaf+"stem boom"
            leaf+(spud here)
            leaf+"{(spud p.i.sour)} has no node"
        ==
      :: %-  (slog tang)
      =.  boom.tidy.kind.node  [~ tang]
      =.  land  (~(put of land) here node)
      this
    ?:  &(?=(%stem -.kind.u.dep) ?=(^ boom.tidy.kind.u.dep))
      :: If a source is a stem and has a boom (crashed on recompute),
      :: we inherit its boom
      ::
      =.  boom.tidy.kind.node  boom.tidy.kind.u.dep
      =.  land  (~(put of land) here node)
      this
    :: if the source last has changed, we are not tidy
    ::
    ?:  (lth q.i.sour last:(need (~(get of trac) p.i.sour)))
      $(tidy %|)
    $(sour t.sour)
  :: If we are not tidy because our sources have updated
  :: recompute here
  ::
  =/  res=(each [darts=(list dart:s) data=vase] tang)
    %-  mule  |.
    =/  =stem:s  (get-stem here)
    =/  deps=(map path vase)  (make-deps ~(key by sour.kind.node))
    (stem [now our eny here deps]:[bowl .])
  ?-    -.res
      %|
    =/  =tang  [leaf+"stem boom" leaf+(spud here) p.res]
    :: %-  (slog tang)
    =.  node  node(tidy.kind [%| ~ tang])
    =.  land  (~(put of land) here node)
    this
      %&
    =?  trac  !=(data.node data.p.res)
      =/  =tack:s  (need (~(get of trac) here))
      (~(put of trac) here tack(last (new-last now.bowl last.tack)))
    =.  node  node(data data.p.res, tidy.kind [%& ~])
    =.  land  (~(put of land) here node)
    (emit-bolts (turn darts.p.res (lead here)))
  ==
::
++  dirty-and-tidy
  |=  here=path
  ^+  this
  ~&  >>  %dirty-and-tidy
  =^  e  land  (dirty here)
  =/  edge=(list path)  ~(tap in e)
  |-
  ?~  edge
    this
  =.  this  (tidy-stem i.edge)
  $(edge t.edge)
::
++  make-root
  |=  [=wire here=path stud=path data=vase]
  ^+  this
  =/  =node:s  [data stud [%root ~]]
  =.  land  (~(put of land) here node)
  =.  trac
    %+  ~(put of trac)  here
    ?~  tac=(~(get of trac) here)
      [now.bowl ~ ~ ~]
    u.tac(last (new-last now.bowl last.u.tac))
  ~&  >>  %here
  (poke-root wire here /init !>(~))
::
++  make-stem
  |=  [here=path stud=path sour=(set path)]
  ^+  this
  ?<  ?=([%fil *] here) :: stems not allowed in /fil
  =/  =node:s  [!>(~) stud [%stem [| ~] ~]]
  =.  land     (~(put of land) here node)
  =.  state    (add-sources here sour)
  =.  trac
    %+  ~(put of trac)  here
    ?~  tac=(~(get of trac) here)
      [now.bowl ~ ~ ~]
    u.tac(last (new-last now.bowl last.u.tac))
  (dirty-and-tidy here)
::
++  poke-root
  |=  [=wire here=path =pail:s]
  ^+  this
  =/  from=path  [(scot %p our.bowl) sap.bowl]
  (take-poke here [from wire] pail)
::
++  bump-root
  |=  [=wire here=path =pail:s]
  ^+  this
  =/  from=path  [(scot %p our.bowl) sap.bowl]
  (take-bump here [from wire] pail)
::
++  kill-root
  |=  here=path
  ^+  this
  (take-kill here)
::
++  take-peek
  |=  [here=path =wire =path]
  ^+  this
  (ingest here ~ %peek wire path (~(dip of land) path))
::
++  take-kill
  |=  here=path
  ^+  this
  =/  =tack:s  (need (~(get of trac) here))
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%root -.kind.node)
  =.  land  (~(put of land) here node(proc.kind ~))
  |-
  =.  this  (give-poke-result here %| %killed [leaf+(spud here) ~])
  =^  poke  trac
    (stage-next-poke here)
  ?^(poke $ this) :: repeat until no poke staged
::
++  take-bump
  |=  [here=path =give:s =pail:s]
  ^+  this
  =/  res=(each _this tang)
    (mule |.((ingest here ~ %bump pail)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  =?  this  ?=([@ %gall %signals ^] from.give)
    %+  ingest
      t.t.t.from.give
    [~ %root wire.give %bump err]
  =?  this  ?=([@ %gall @ ~] from.give)
    =/  =wire  (weld /vent/[i.from.give] wire.give)
    %-  emit-cards
    ~&  >>>  from+from.give
    ~&  >>>  giving-bump-result+[wire]
    :~  [%give %fact ~[wire] sign-root+!>([%bump err])]
        [%give %kick ~[wire] ~]
    ==
  this
:: TODO: cleanup on process crash or kill
::
++  take-poke
  |=  [here=path =poke:s]
  ^+  this
  ~&  %take-poke
  ~&  here+here
  ~&  poke+poke
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
  ^+  this
  ~&  %run-next-poke
  ~&  here+here
  =/  =node:s  (need (~(get of land) here))
  =^  poke  trac
    (stage-next-poke here)
  ?~  poke 
    this
  ?>  ?=(%root -.kind.node)
  =/  =bowl:root:s  [now our eny from.give.u.poke here]:[bowl .]
  =/  build=(each proc:root:s tang)
    (mule |.(((get-root here) bowl pail.u.poke)))
  ?:  ?=(%& -.build)
    =.  land  (~(put of land) here node(proc.kind `p.build))
    (ingest here ~) :: start
  :: %-  (slog [leaf+"build-fail" leaf+(spud here) p.build])
  =.  this  (give-poke-result here %| %build-fail p.build)
  (run-next-poke here)
::
++  give-poke-result
  |=  [here=path res=(each pail:s goof)]
  ^+  this
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%root -.kind.node)
  =/  =tack:s  (need (~(get of trac) here))
  ?>  ?=(^ give.tack)
  =.  land  (~(put of land) here node(proc.kind ~))
  =.  trac  (~(put of trac) here tack(give ~))
  ?:  ?=([@ %gall %signals ^] from.u.give.tack)
    %+  ingest
      t.t.t.from.u.give.tack
    [~ %root wire.u.give.tack %poke res]
  ?>  ?=([@ %gall @ ~] from.u.give.tack)
  =/  =wire  (weld /vent/[i.from] wire):[u.give.tack .]
  %-  emit-cards
  :~  [%give %fact ~[wire] sign-root+!>([%poke res])]
      [%give %kick ~[wire] ~]
  ==
::
++  ingest
  |=  [here=path in=(unit intake:root:s)]
  ^+  this
  ~&  %ingest
  ~&  here+here
  ~&  in+in
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%root -.kind.node)
  ?>  ?=(^ proc.kind.node)
  =/  =bowl:root:s  [now our eny sap here]:[bowl .]
  =/  m  (charm:root:s ,pail:s)
  =/  res=(each [[(list dart:s) vase result:eval:m] proc:root:s] tang)
    (mule |.((take:eval:m u.proc.kind.node bowl data.node in)))
  =/  [[darts=(list dart:s) data=vase =result:eval:m] =proc:root:s]
    ?-  -.res
      %&  p.res
      %|  [[~ data.node [%fail %crash [leaf+(spud here) p.res]]] u.proc.kind.node]
    ==
  ::
  ~&  result+result
  ::
  =.  this  (emit-bolts (turn darts (lead here)))
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
  =?  this  tick  (dirty-and-tidy here)
  ::
  ?:  ?=(%next -.result)
    this
  =.  this
    %+  give-poke-result
      here
    ?-  -.result
      %fail  [%| err.result]
      %done  [%& value.result]
    ==
  (run-next-poke here)
::
++  wrap-wire
  |=  [here=path =wire]
  ^+  wire
  ;:  weld
    /root/(scot %ud (lent here))
    here  wire
  ==
::
++  unwrap-wire
  |=  =wire
  ^-  [path ^wire]
  ?>  ?=([%root @ *] wire)
  =/  len=@ud  (slav %ud i.t.wire)
  [(scag len t.t.wire) (slag len t.t.wire)]
::
++  wrap-sysc-card
  |=  [here=path =card:agent:gall]
  ^+  card
  ?+    card  card
      [%pass * *]  [%pass (wrap-wire here p.card) q.card]
      [%give ?(%fact %kick) *]
    =-  card(paths.p -)
    (turn paths.p.card (cury wrap-wire here))
  ==
::
++  take-arvo
  |=  [wir=wire sign=sign-arvo]
  ^+  this
  =/  [here=path =wire]  (unwrap-wire wir)
  (ingest here ~ %arvo wire sign)
::
++  take-agent
  |=  [wir=wire =sign:agent:gall]
  ^+  this
  =/  [here=path =wire]  (unwrap-wire wir)
  (ingest here ~ %agent wire sign)
::
++  take-watch
  |=  pat=path
  ^+  this
  =/  [here=path =path]  (unwrap-wire pat)
  (ingest here ~ %watch path)
::
++  take-leave
  |=  pat=path
  ^+  this
  =/  [here=path =path]  (unwrap-wire pat)
  (ingest here ~ %leave path)
--
