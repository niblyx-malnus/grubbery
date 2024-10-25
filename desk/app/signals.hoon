/-  s=signals
/+  signals, server, dbug, verb, default-agent
/=  x-  /ted/boot
/=  x-  /mar/sign-root
|%
+$  state-0  [%0 =land:s =trac:s =bindings:s]
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
      %handle-http-request
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    =/  [[* site=(list @t)] *]
      (parse-request-line:server url.request.req)
    =/  prefix=(list @t)  (scag 1 site)
    |-
    ?~  here=(~(get by bindings) prefix)
      $(prefix (scag +((lent prefix)) site))
    =/  =give:s  [[(scot %p our.bowl) sap.bowl] /]
    =^  cards  state
      abet:(poke-root:hc u.here give /handle-http-request !>(req))
    [cards this]
    ::
      %oust-node
    =+  !<(here=path vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %oust)
    =/  =give:s  [[(scot %p our.bowl) sap.bowl] /]
    =^  cards  state
      abet:(oust-node:hc give here)
    [cards this]
    ::
      %make-root
    =+  !<([here=path stud=path data=^vase] vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %make)
    =/  =give:s  [[(scot %p our.bowl) sap.bowl] /]
    =^  cards  state
      abet:(make-root:hc give here stud data)
    [cards this]
    ::
      %make-stem
    =+  !<([here=path stud=path sour=(set path)] vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %make)
    =/  =give:s  [[(scot %p our.bowl) sap.bowl] /]
    =^  cards  state
      abet:(make-stem:hc give here stud sour)
    [cards this]
    ::
      %poke-root
    =+  !<([=wire here=path =pail:s] vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %poke)
    =/  =give:s  [[(scot %p our.bowl) sap.bowl] wire]
    =^  cards  state
      abet:(poke-root:hc here give pail)
    [cards this]
    ::
      %bump-root
    =+  !<([here=path =pail:s] vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %bump)
    =/  =give:s  [[(scot %p our.bowl) sap.bowl] /]
    =^  cards  state
      abet:(bump-root:hc here give pail)
    [cards this]
    ::
      %kill-root
    =+  !<(here=path vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %kill)
    =/  =give:s  [[(scot %p our.bowl) sap.bowl] /]
    =^  cards  state
      abet:(kill-root:hc give here)
    [cards this]
    ::
      %tidy-stem
    =+  !<(here=path vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %tidy)
    =/  =give:s  [[(scot %p our.bowl) sap.bowl] /]
    =^  cards  state
      abet:(tidy-stem:hc give here)
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
      [%poke @ *]
    ?>  =(src.bowl (slav %p i.t.path))
    [~ this]
    ::
      [%http-response *]
    [~ this]
  ==
::
++  on-leave
  |=  =path
  ^-  (quip card _this)
  ?.  ?=([%root @ *] path)
    (on-leave:def path)
  =^  cards  state
    abet:(take-leave path)
  [cards this]
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
  ?+    wire  (on-arvo:def wire sign)
      [%root @ *]
    =^  cards  state
      abet:(take-arvo:hc wire sign)
    [cards this]
  ==
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
  =/  [here=path =dart:s]  ?~(bolts !! i.bolts)
  =.  bolts  ?~(bolts !! t.bolts)
  =/  from=path  (weld /(scot %p our.bowl)/gall/[dap.bowl]/$ here)
  ?-    -.dart
      %sysc
    :: TODO: keep track of scrying with %keen so you can
    ::       so you can cancel with %yawn when you kill
    ::       a process or when it crashes
    ::
    $(this (handle-sysc-card here card.dart))
      %node
    ?-    -.load.dart
        %poke
      $(this (poke-root path [from wire] pail.load):[dart .])
      ::
        %bump
      $(this (bump-root path [from wire] pail.load):[dart .])
      ::
        %peek
      $(this (take-peek here [wire path]:dart))
      ::
        %make
      =/  =give:s  [from wire.dart]
      ?-  -.make.load.dart
        %root  $(this (make-root give [path pail.make.load]:dart))
        %stem  $(this (make-stem give [path [stud sour]:make.load]:dart))
      ==
      ::
        %oust
      $(this (oust-node [from wire] path):[dart .])
      ::
        %kill
      $(this (kill-root [from wire] path):[dart .])
      ::
        %tidy
      $(this (tidy-stem [from wire] path):[dart .])
    ==
  ==
::
++  new-last
  |=  [now=@da last=@da]
  ^-  @da
  ?:  (lth last now)
    now
  +(last)
::
++  next-tack
  |=  here=path
  ^+  this
  =/  =tack:s  (need (~(get of trac) here))
  =.  tack     tack(step.last (new-last now.bowl step.last.tack))
  ?:  &(=(~ sinx.tack) !(~(has of land) here))
    this(trac (~(del of trac) here)) :: may occur in an oust
  this(trac (~(put of trac) here tack))
:: +decap from /lib/rudder
::
++  has-prefix
  |=  [head=path =path]
  ^-  ?
  ?~  head  %&
  ?~  path  %|
  ?.  =(i.head i.path)  %|
  $(head t.head, path t.path)
:: Cannot talk to system and cannot talk outside cone
::
++  strict-sandbox
  |=  here=path
  ^-  sand:s
  |=  dest=(unit [=path *])
  ^-  ?
  ?~  dest
    %|
  (has-prefix here path.u.dest)
::
++  get-sand
  |=  here=path
  ^-  sand:s
  ?~  here  _& :: root can talk to anyone
  ?~  nod=(~(get of land) [%san here])  _& :: can be bubbled through
  ?>  ?=(%stem -.kind.u.nod)
  ?.  =([%& ~] tidy.kind.u.nod)  (strict-sandbox here)
  =/  res=(each sand:s tang)  (mule |.(!<(sand:s data.u.nod)))
  ?-  -.res
    %&  p.res
    %|  (strict-sandbox here) :: emergency sandbox
  ==
:: check if a dart can leave its cone
::
++  sand-bubble
  |=  [here=path dest=(unit [path deed:s])]
  ^-  ?
  =/  =sand:s  (get-sand here)
  ?.  (sand dest)
    %|
  ?:  =(~ here)
    %&
  $(here (snip here))
::
++  get-acol
  |=  here=path
  ^-  acol:s
  :: files in /acl define access control on corresponding file in /san
  ?:  ?=([%san *] here)
    $(here [%acl t.here])
  :: files in /acl define access control on themselves as well
  ?.  ?=([%acl *] here)
    $(here [%acl here])
  ?~  nod=(~(get of land) here)  _|
  ?>  ?=(%stem -.kind.u.nod)
  ?.  =([%& ~] tidy.kind.u.nod)  _|
  :: TODO: does mule catch evil vases???
  =/  res=(each acol:s tang)
    (mule |.(!<(acol:s data.u.nod)))
  ?-(-.res %| _|, %& p.res) :: default private
:: check if a foreign ship can perform an action
::
++  acol-tunnel
  |=  [here=path poke=[=ship deed:s]]
  ^-  ?
  ?:  =(ship.poke our.bowl)
    %&
  =|  prefix=path
  |-
  =/  =acol:s  (get-acol prefix)
  ?:  (acol poke)
    %&
  ?:  =(here prefix)
    %|
  $(prefix (scag +((lent prefix)) here))
::
++  get-root
  |=  here=path
  ^-  root:s
  ?<  ?=([%bin *] here) :: all /bin nodes are stems
  ?:  ?=([%lib *] here)  root:lib:signals :: /lib nodes
  =/  =node:s  (need (~(get of land) (welp /bin/nod here)))
  ?>  ?=(%stem -.kind.node)
  ?>  =([%& ~] tidy.kind.node)
  !<(root:s data.node)
::
++  get-stem
  |=  here=path
  ^-  stem:s
  ?<  ?=([%lib *] here) :: all /lib nodes are roots
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
  ?:  ?=(%root -.kind.u.nod)
    %&
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
  ^+  this
  =/  sour=(list path)  ~(tap in sour)
  ?>  (no-cycles here sour)
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%stem -.kind.node)
  |-
  ?~  sour
    this(land (~(put of land) here node))
  ?.  (~(has of land) i.sour)
    ~|(source+i.sour !!)
  =/  =tack:s  (need (~(get of trac) i.sour))
  =.  sinx.tack  (~(put in sinx.tack) here)
  =.  trac  (~(put of trac) i.sour tack)
  =.  sour.kind.node  (~(put by sour.kind.node) i.sour step.last.tack)
  $(sour t.sour)
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
  |=  [here=path sour=(set path)]
  ^-  (map path vase)
  %-  ~(gas by *(map path vase))
  %+  turn  ~(tap in sour)
  |=  =path
  ?>  (sand-bubble here ~ path %peek)
  :-  path
  data:(need (~(get of land) path))
::
++  tidy-stem
  |=  [=give:s here=path]
  ^+  this
  ~&  >>  %tidy-stem
  =/  res=(each _this tang)
    (mule |.((tidy here)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %signals %$ ^] from.give)
    ?:(?=(%| -.res) !! this)
  %^    ingest
      [(scot %p our.bowl) /gall/signals]
    t.t.t.t.from.give
  [~ %tidy wire.give err]
::
++  tidy
  |=  here=path
  ^+  this
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
    =.  this  (tidy p.i.sour)
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
    :: if the source step.last has changed, we are not tidy
    ::
    ?:  (lth q.i.sour step.last:(need (~(get of trac) p.i.sour)))
      $(tidy %|)
    $(sour t.sour)
  :: If we are not tidy because our sources have updated
  :: recompute here
  ::
  =/  res=(each [darts=(list dart:s) data=vase] tang)
    %-  mule  |.
    =/  =stem:s  (get-stem here)
    =/  deps=(map path vase)
      (make-deps here ~(key by sour.kind.node)) :: sandoxed deps
    (stem [now our eny here deps]:[bowl .])
  ?-    -.res
      %|
    =/  =tang  [leaf+"stem boom" leaf+(spud here) p.res]
    :: %-  (slog tang)
    =.  node  node(tidy.kind [%| ~ tang])
    =.  land  (~(put of land) here node)
    this
      %&
    =?  this  !=(data.node data.p.res)  (next-tack here)
    =.  node  node(data data.p.res, tidy.kind [%& ~])
    =.  land  (~(put of land) here node)
    %-  emit-bolts
    %+  turn  darts.p.res
    |=  =dart:s
    ?-    -.dart
        %sysc
      ~|  "ERROR: {(spud here)} has no system access"
      ?>  (sand-bubble here ~)
      [here dart]
      ::
        %node
      ~|  "ERROR: {(spud here)} has no {(trip -.load.dart)} access to {(spud path.dart)}"
      ?>  (sand-bubble here ~ [path -.load]:dart)
      [here dart]
    ==
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
  =.  this  (tidy i.edge)
  $(edge t.edge)
::
++  del-sources
  |=  here=path
  ^+  this
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%stem -.kind.node)
  =/  sour=(list path)  (turn ~(tap in sour.kind.node) head)
  |-
  ?~  sour
    this
  =/  =tack:s    (need (~(get of trac) i.sour))
  =.  sinx.tack  (~(del in sinx.tack) here)
  ?:  &(=(~ sinx.tack) !(~(has of land) i.sour))
    $(sour t.sour, trac (~(del of trac) i.sour))
  $(sour t.sour, trac (~(put of trac) i.sour tack))
::
++  do-oust
  |=  here=path
  ^+  this
  =/  =node:s  (need (~(get of land) here))
  =.  this
    ?-  -.kind.node
      %root  (kill here)
      %stem  (del-sources here)
    ==
  =.  land  (~(del of land) here)
  (next-tack here)
::
++  oust-node
  |=  [=give:s here=path]
  ^+  this
  =/  res=(each _this tang)  (mule |.((do-oust here)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %signals %$ ^] from.give)
    ?:(?=(%| -.res) !! this)
  %^    ingest
      [(scot %p our.bowl) /gall/signals]
    t.t.t.t.from.give
  [~ %gone wire.give err]
::
++  new-root
  |=  [here=path stud=path data=vase]
  ^+  this
  ?<  ?=([%bin *] here) :: roots not allowed in /bin
  ?<  ?=([%acl *] here) :: roots not allowed in /acl
  ?<  ?=([%san *] here) :: roots not allowed in /san
  =/  =node:s  [data stud [%root ~]]
  =.  land  (~(put of land) here node)
  =.  trac
    %+  ~(put of trac)  here
    ?~  tac=(~(get of trac) here)
      [[now now]:bowl ~ ~ ~ ~]
    u.tac(step.last (new-last now.bowl step.last.u.tac))
  (dirty-and-tidy here)
::
++  make-root
  |=  [=give:s here=path stud=path data=vase]
  ^+  this
  =/  res=(each _this tang)  (mule |.((new-root here stud data)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %signals %$ ^] from.give)
    ?:(?=(%| -.res) !! this)
  %^    ingest
      [(scot %p our.bowl) /gall/signals]
    t.t.t.t.from.give
  [~ %made wire.give err]
::
++  new-stem
  |=  [here=path stud=path sour=(set path)]
  ^+  this
  ?<  ?=([%lib *] here) :: stems not allowed in /lib
  =/  =node:s  [!>(~) stud [%stem [| ~] ~]]
  =.  land     (~(put of land) here node)
  =.  this     (add-sources here sour)
  =.  trac
    %+  ~(put of trac)  here
    ?~  tac=(~(get of trac) here)
      [[now now]:bowl ~ ~ ~ ~]
    u.tac(step.last (new-last now.bowl step.last.u.tac))
  (dirty-and-tidy here)
::
++  make-stem
  |=  [=give:s here=path stud=path sour=(set path)]
  ^+  this
  =/  res=(each _this tang)  (mule |.((new-stem here stud sour)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %signals %$ ^] from.give)
    ?:(?=(%| -.res) !! this)
  %^    ingest
      [(scot %p our.bowl) /gall/signals]
    t.t.t.t.from.give
  [~ %made wire.give err]
::
++  take-peek
  |=  [here=path =wire pat=path]
  ^+  this
  =/  from=path  [(scot %p our.bowl) /gall/signals]
  (ingest from here ~ %peek wire pat (~(dip of land) pat))
::
++  kill
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
++  kill-root
  |=  [=give:s here=path]
  ^+  this
  =/  res=(each _this tang)  (mule |.((kill here)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %signals %$ ^] from.give)
    ?:(?=(%| -.res) !! this)
  %^    ingest
      [(scot %p our.bowl) /gall/signals]
    t.t.t.t.from.give
  [~ %dead wire.give err]
::
++  bump-root
  |=  [here=path =give:s =pail:s]
  ^+  this
  =/  res=(each _this tang)
    (mule |.((ingest from.give here ~ %bump pail)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %signals %$ ^] from.give)
    ?:(?=(%| -.res) !! this)
  %^    ingest
      [(scot %p our.bowl) /gall/signals]
    t.t.t.t.from.give
  [~ %root wire.give %bump err]
:: TODO: handle outgoing keens
::
++  clean
  |=  [here=path last=@da]
  ^+  this
  %-  emit-cards
  %+  murn  ~(tap by wex.bowl)
  |=  [[=wire =ship =term] *]
  ^-  (unit card)
  ?.  ?=([%root @ *] wire)
    ~
  =/  [h=path l=@da *]  (unwrap-wire wire)
  ?.  &(=(h here) =(l last))
    ~
  [~ %pass wire %agent [ship term] %leave ~]
::
++  poke-root
  |=  [here=path =poke:s]
  ^+  this
  ~&  %take-poke
  =/  =tack:s  (need (~(get of trac) here))
  =.  trac  (~(put of trac) here tack(line (~(put to line.tack) poke)))
  (run-next-poke here)
::
++  stage-next-poke
  |=  here=path
  ^-  [(unit poke:s) trac:s]
  =/  =tack:s  (need (~(get of trac) here))
  ?>  =(~ give.tack)
  ?:  =(~ line.tack)
    [~ trac] :: no more pokes to run
  =/  [=poke:s line=(qeu poke:s)]  ~(get to line.tack)
  =.  tack
    %=  tack
      line       line
      give       [~ give.poke]
      poke.last  (new-last now.bowl poke.last.tack)
    ==
  [`poke (~(put of trac) here tack)]
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
  =/  =bowl:root:s  [now our eny ~ ~ from.give.u.poke here]:[bowl .]
  =/  build=(each proc:root:s tang)
    (mule |.(((get-root here) bowl pail.u.poke)))
  ?:  ?=(%& -.build)
    =.  land  (~(put of land) here node(proc.kind `p.build))
    =/  from=path  [(scot %p our.bowl) /gall/signals]
    (ingest from here ~) :: start
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
  =.  this  (clean here poke.last.tack)
  ?:  ?=([@ %gall %signals %$ ^] from.u.give.tack)
    %^    ingest
        [(scot %p our.bowl) /gall/signals]
      t.t.t.t.from.u.give.tack
    [~ %root wire.u.give.tack %poke res]
  ?>  ?=([@ %gall @ ~] from.u.give.tack)
  ?~  eyre.tack
    =/  =wire  (weld /poke/[i.from] wire):[u.give.tack .]
    %-  emit-cards
    :~  [%give %fact ~[wire] sign-root+!>([%poke res])]
        [%give %kick ~[wire] ~]
    ==
  =;  rep=http-response:s
    =/  =wire  /http-response/[u.eyre.tack]
    %-  emit-cards
    :~  [%give %fact ~[wire] %http-response-header !>(p.rep)]
        [%give %fact ~[wire] %http-response-data !>(q.rep)]
        [%give %kick ~[wire] ~]
    ==
  ?-    -.res
      %&
    ?.  ?=([%http-response ~] p.p.res)
      =/  data=octs
        (as-octs:mimes:html '<h1>500 Bad Root Output</h1>')
      =/  content-length=@t  (crip ((d-co:co 1) p.data))
      :_  `data
      :-  500
      :~  ['Content-Length' content-length]
          ['Content-Type' 'text/html']
          ['Allow' 'GET']
      ==
    =/  output  (mule |.(!<(http-response:s q.p.res)))
    ?:  ?=(%& -.output)
      p.output
    =/  data=octs
      (as-octs:mimes:html '<h1>500 Bad Root Output</h1>')
    =/  content-length=@t  (crip ((d-co:co 1) p.data))
    :_  `data
    :-  500
    :~  ['Content-Length' content-length]
        ['Content-Type' 'text/html']
        ['Allow' 'GET']
    ==
    ::
      %|
    =/  =acol:s  (get-acol here)
    ?.  (acol src.bowl %peek)
      =/  data=octs
        (as-octs:mimes:html '<h1>500 Root Crashed</h1>')
      =/  content-length=@t  (crip ((d-co:co 1) p.data))
      :_  `data
      :-  500
      :~  ['Content-Length' content-length]
          ['Content-Type' 'text/html']
          ['Allow' 'GET']
      ==
    :: TODO: Also add tang and generally clean up
    =/  data=octs
      (as-octs:mimes:html '<h1>500 Root Crashed</h1>')
    =/  content-length=@t  (crip ((d-co:co 1) p.data))
    :_  `data
    :-  500
    :~  ['Content-Length' content-length]
        ['Content-Type' 'text/html']
        ['Allow' 'GET']
    ==
  ==
::
++  make-bowl
  |=  [from=path here=path]
  ^-  bowl:root:s
  =/  last=@da  poke.last:(need (~(get of trac) here))
  =.  wex.bowl
    %-  ~(gas by *boat:gall)
    %+  murn  ~(tap by wex.bowl)
    |=  [[=wire =ship =term] acked=? pat=path]
    ?.  ?=([%root @ *] wire)
      ~
    =/  [h=path l=@da w=path]  (unwrap-wire wire)
    ?.  &(=(h here) =(l last))
      ~
    [~ [w ship term] acked pat]
  =.  sup.bowl
    %-  ~(gas by *bitt:gall)
    %+  murn  ~(tap by sup.bowl)
    |=  [=duct =ship pat=path]
    ?.  ?=([%root @ *] pat)
      ~
    =/  [h=path l=@da p=path]  (unwrap-wire pat)
    ?.  &(=(h here) =(l last))
      ~
    [~ duct ship p]
  [now our eny wex sup from here]:[bowl .]
::
++  ingest
  |=  [from=path here=path in=(unit intake:root:s)]
  ^+  this
  ~&  %ingest
  ~&  here+here
  =/  =node:s  (need (~(get of land) here))
  ?>  ?=(%root -.kind.node)
  ?>  ?=(^ proc.kind.node)
  =/  m  (charm:root:s ,pail:s)
  =/  =bowl:root:s  (make-bowl from here)
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
  =.  this
    %-  emit-bolts
    %+  turn  darts
    |=  =dart:s
    ?-    -.dart
        %sysc
      ~|  "ERROR: {(spud here)} has no system access"
      ?>  (sand-bubble here ~)
      [here dart]
      ::
        %node
      ~|  "ERROR: {(spud here)} has no {(trip -.load.dart)} access to {(spud path.dart)}"
      ?>  (sand-bubble here ~ [path -.load]:dart)
      [here dart]
    ==
  ::
  =/  tick=?  !=(data data.node)
  =?  this  tick  (next-tack here)
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
  |=  [here=path last=@da =wire]
  ^+  wire
  ;:  weld
    /root/(scot %ud (lent here))
    here  /(scot %da last)  wire
  ==
::
++  unwrap-wire
  |=  =wire
  ^-  [path @da ^wire]
  ?>  ?=([%root @ *] wire)
  =/  len=@ud  (slav %ud i.t.wire)
  :+  (scag len t.t.wire)
    (slav %da (snag len t.t.wire))
  (slag +(len) t.t.wire)
::
++  handle-sysc-card
  |=  [here=path =card:agent:gall]
  ^+  this
  =/  last=@da  poke.last:(need (~(get of trac) here))
  ?+    card  (emit-card card)
      [%give ?(%fact %kick) *]
    =-  (emit-card card(paths.p -))
    (turn paths.p.card |=(p=path (wrap-wire here last p)))
    ::
      [%pass * *]
    =.  bindings
      ?+    card
        bindings
          [%pass * %arvo %e %disconnect *]
        (~(del by bindings) path.binding.q.card)
        ::
          [%pass * %arvo %e %connect * %signals]
        (~(put by bindings) path.binding.q.card here)
      ==
    (emit-card [%pass (wrap-wire here last p.card) q.card])
  ==
::
++  take-arvo
  |=  [wir=wire sign=sign-arvo]
  ^+  this
  =/  [here=path last=@da =wire]  (unwrap-wire wir)
  =/  curr=@da  poke.last:(need (~(get of trac) here))
  ?.  =(last curr)
    ~&  >>  "discarding message for old root process"
    this
  =/  from=path  [(scot %p our.bowl) sap.bowl]
  (ingest from here ~ %arvo wire sign)
::
++  take-agent
  |=  [wir=wire =sign:agent:gall]
  ^+  this
  =/  [here=path last=@da =wire]  (unwrap-wire wir)
  =/  curr=@da  poke.last:(need (~(get of trac) here))
  ?.  =(last curr)
    ~&  >>  "discarding message for old root process"
    this
  =/  from=path  [(scot %p our.bowl) sap.bowl]
  (ingest from here ~ %agent wire sign)
::
++  take-watch
  |=  pat=path
  ^+  this
  =/  [here=path last=@da pan=path]  (unwrap-wire pat)
  =/  curr=@da  poke.last:(need (~(get of trac) here))
  ?.  =(last curr)
    ~&  >>  "discarding message for old root process"
    this
  =/  from=path  [(scot %p our.bowl) sap.bowl]
  (ingest from here ~ %watch pan)
::
++  take-leave
  |=  pat=path
  ^+  this
  =/  [here=path last=@da pan=path]  (unwrap-wire pat)
  =/  from=path  [(scot %p our.bowl) sap.bowl]
  =/  curr=@da  poke.last:(need (~(get of trac) here))
  ?.  =(last curr)
    ~&  >>  "discarding message for old root process"
    this
  (ingest from here ~ %leave pan)
--
