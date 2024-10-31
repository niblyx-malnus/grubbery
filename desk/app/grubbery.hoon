/-  g=grubbery
/+  grubbery, io=grubberyio, server, dbug, verb, default-agent
/=  x-  /mar/sign-base
|%
+$  state-0  [%0 =cone:g =trac:g =bindings:g =history:g]
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
  =^  cards  state
    abet:boot:hc
  [cards this]
::
++  on-save   !>(state)
::
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  :: TODO: make sure we add a /bin/zuse with !>(..zuse)
  ::       every time we boot
  :: TODO: make sure we kill all processes everytime we load
  ::       (might need to send poke responses outside the agent)
  ::
  =.  state  !<(state-0 old)
  =^  cards  state
    abet:boot:hc
  [cards this]
::
++  on-peek
  |=  =(pole knot)
  ^-  (unit (unit cage))
  ?.  ?=([%x %history since=@ta ~] pole)
    [~ ~]
  =/  since=@da  (slav %da since.pole)
  ``noun+!>((tap:hon:g (lot:hon:g history ~ `since)))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ~&  "poke to {<dap.bowl>} agent with mark {<mark>}"
  ?+    mark  (on-poke:def mark vase)
      %connect
    ?>  =(src our):bowl
    =+  !<([url=path =path] vase)
    :_  this
    [%pass [%connect path] %arvo %e %connect `url %grubbery]~
    ::
      %disconnect
    ?>  =(src our):bowl
    =+  !<(url=path vase)
    :-  [%pass / %arvo %e %disconnect `url]~
    this(bindings (~(del by bindings) url))
    ::
      %handle-http-request
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    =/  lin=request-line:server
      (parse-request-line:server url.request.req)
    =/  prefix=(list @t)  (scag 1 site.lin)
    |-
    ?~  here=(~(get by bindings) prefix)
      ?.  (lth (lent prefix) (lent site.lin))
        ~&("strange url: {(spud site.lin)}" [~ this])
      $(prefix (scag +((lent prefix)) site.lin))
    =/  suffix=path  (slag (lent prefix) site.lin)
    :: send the request to the base grub with the longest
    :: corresponding prefix
    ::
    =/  dest=path  (weld u.here suffix)
    =.  dest
      |-
      ?^  get=(~(get of cone) dest)
        ?:  ?=(%base -.kind.u.get)
          dest
        $(dest (snip dest))
      $(dest (snip dest))
    ::
    =/  =give:g
      :_  /
      ;:  weld
        /(scot %p our.bowl)
        sap.bowl
        /(scot %p src.bowl)
        /[eyre-id]
      ==
    ::
    =/  =pail:g  [/handle-http-request !>([lin req])]
    =^  cards  state
      abet:(poke-base:hc dest give pail)
    [cards this]
    ::
      %oust-grub
    =+  !<(here=path vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %oust)
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(oust-grub:hc give here)
    [cards this]
    ::
      %make-base
    =+  !<([here=path stud=path base=path data=(unit ^vase)] vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %make)
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(make-base:hc give here stud base data)
    [cards this]
    ::
      %make-stem
    =+  !<([here=path stud=path stem=path sour=(set path)] vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %make)
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(make-stem:hc give here stud stem sour)
    [cards this]
    ::
      %poke-base
    =+  !<([=wire here=path =pail:g] vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %poke)
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] wire]
    =^  cards  state
      abet:(poke-base:hc here give pail)
    [cards this]
    ::
      %bump-base
    =+  !<([here=path =pail:g] vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %bump)
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(bump-base:hc here give pail)
    [cards this]
    ::
      %kill-base
    =+  !<(here=path vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %kill)
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(kill-base:hc give here)
    [cards this]
    ::
      %tidy-stem
    =+  !<(here=path vase)
    ~&  here+here
    ?>  (acol-tunnel:hc here src.bowl %tidy)
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(tidy-stem:hc give here)
    [cards this]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
      [%base @ *]
    =^  cards  state
      abet:(take-watch path)
    [cards this]
    ::
      [%poke @ *]
    ?>  =(src.bowl (slav %p i.t.path))
    [~ this]
    ::
      [%http-response *]
    :: ~&  >  "eyre subscribing to http-response"
    :: ~&  src+src.bowl
    :: ~&  sap+sap.bowl
    :: ?>  =(src our):bowl :: ?
    [~ this]
  ==
::
++  on-leave
  |=  =path
  ^-  (quip card _this)
  ?.  ?=([%base @ *] path)
    (on-leave:def path)
  =^  cards  state
    abet:(take-leave path)
  [cards this]
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?.  ?=([%base @ *] wire)
    (on-agent:def wire sign)
  =^  cards  state
    abet:(take-agent:hc wire sign)
  [cards this]
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  ?+    wire  (on-arvo:def wire sign)
      [%base @ *]
    =^  cards  state
      abet:(take-arvo:hc wire sign)
    [cards this]
    ::
      [%connect *]
    ?>  ?=([%eyre %bound *] sign)
    ?.  accepted.sign
      %-  (slog leaf+"Binding {(spud path.binding.sign)} to {(spud t.wire)} failed!" ~)
      [~ this]
    %-  (slog leaf+"{(spud path.binding.sign)} bound successfully to {(spud t.wire)}!" ~)
    [~ this(bindings (~(put by bindings) path.binding.sign t.wire))]
  ==
::
++  on-fail   on-fail:def
--
::
=|  cards=(list card)
=|  bolts=(list bolt:g) :: a stack
|_  =bowl:gall
+*  this  .
++  emit-card   |=(=card this(cards [card cards]))
++  emit-cards  |=(cadz=(list card) this(cards (welp (flop cadz) cards)))
++  emit-bolt   |=(=bolt:g this(bolts [bolt bolts]))
++  emit-bolts  |=(botz=(list bolt:g) this(bolts (welp botz bolts)))
:: handle all bolts and return effects and state
::
++  abet
  |-
  ?~  =(~ bolts)
    [(flop cards) state]
  =/  [here=path =dart:g]  ?~(bolts !! i.bolts)
  =.  bolts  ?~(bolts !! t.bolts)
  =/  from=path  (weld /(scot %p our.bowl)/gall/[dap.bowl]/$ here)
  ?-    -.dart
      %sysc
    :: TODO: keep track of scrying with %keen so you can
    ::       so you can cancel with %yawn when you kill
    ::       a process or when it crashes
    ::
    $(this (handle-sysc-card here card.dart))
    ::
      %scry
    $(this (take-scry here [wire mold path]:dart))
    ::
      %grub
    ?-    -.load.dart
        %poke
      $(this (poke-base path [from wire] pail.load):[dart .])
      ::
        %bump
      $(this (bump-base path [from wire] pail.load):[dart .])
      ::
        %peek
      $(this (take-peek here [wire path]:dart))
      ::
        %make
      =/  =give:g  [from wire.dart]
      ?-  -.make.load.dart
        %base  $(this (make-base give [path [stud base data]:make.load]:dart))
        %stem  $(this (make-stem give [path [stud stem sour]:make.load]:dart))
      ==
      ::
        %oust
      $(this (oust-grub [from wire] path):[dart .])
      ::
        %kill
      $(this (kill-base [from wire] path):[dart .])
      ::
        %tidy
      $(this (tidy-stem [from wire] path):[dart .])
    ==
  ==
::
++  boot
  ^-  _this
  =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
  =.  this  (oust-grub give /boot)
  =.  this  (make-base give /boot /sig /boot ~)
  (poke-base /boot give /sig !>(~))
::
++  new-last
  |=  [now=@da last=@da]
  ^-  @da
  =/  next=@da  ?:((lth last now) now +(last))
  |-
  ?.  (has:hon:g history next)
    next
  $(next +(next))
::
++  next-tack
  |=  here=path
  ^+  this
  ?~  tac=(~(get of trac) here)
    =/  step=@da  (new-last [now now]:bowl)
    =.  history  (put:hon:g history step here)
    this(trac (~(put of trac) here [step step] ~ ~ ~))
  =^  del  history
    (del:hon:g history step.last.u.tac)
  ?:  &(=(~ sinx.u.tac) !(~(has of cone) here))
    this(trac (~(del of trac) here)) :: may occur in an oust
  =/  step=@da  (new-last now.bowl step.last.u.tac)
  =.  history  (put:hon:g history step here)
  this(trac (~(put of trac) here u.tac(step.last step)))
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
  ^-  sand:g
  |=  dest=(unit [=path *])
  ^-  ?
  ?~  dest
    %|
  (has-prefix here path.u.dest)
::
++  get-sand
  |=  here=path
  ^-  sand:g
  ?~  here  _& :: / can talk to anyone
  ?~  nod=(~(get of cone) [%san here])  _& :: can be bubbled through
  ?>  ?=(%stem -.kind.u.nod)
  ?.  =([%& ~] tidy.kind.u.nod)  (strict-sandbox here)
  =/  res=(each sand:g tang)  (mule |.(!<(sand:g data.u.nod)))
  ?-  -.res
    %&  p.res
    %|  (strict-sandbox here) :: emergency sandbox
  ==
:: check if a dart can leave its cone
::
++  sand-bubble
  |=  [here=path dest=(unit [path deed:g])]
  ^-  ?
  =/  =sand:g  (get-sand here)
  ?.  (sand dest)
    %|
  ?:  =(~ here)
    %&
  $(here (snip here))
::
++  get-acol
  |=  here=path
  ^-  acol:g
  :: files in /acl define access control on corresponding file in /san
  ?:  ?=([%san *] here)
    $(here [%acl t.here])
  :: files in /acl define access control on themselves as well
  ?.  ?=([%acl *] here)
    $(here [%acl here])
  ?~  nod=(~(get of cone) here)  _| :: only you can access / 
  ?>  ?=(%stem -.kind.u.nod)
  ?.  =([%& ~] tidy.kind.u.nod)  _| :: don't use outdated code
  :: TODO: does mule catch evil vases???
  =/  res=(each acol:g tang)
    (mule |.(!<(acol:g data.u.nod)))
  ?-(-.res %| _|, %& p.res) :: default private
:: check if a foreign ship can perform an action
::
++  acol-tunnel
  |=  [here=path poke=[=ship deed:g]]
  ^-  ?
  ?:  =(ship.poke our.bowl)
    %&
  =|  prefix=path
  |-
  =/  =acol:g  (get-acol prefix)
  ?:  (acol poke)
    %&
  ?:  =(here prefix)
    %|
  $(prefix (scag +((lent prefix)) here))
::
++  get-base
  |=  base=path
  ^-  base:g
  ?:  ?=([%boot ~] base)  boot:grubbery
  ?:  ?=([%lib ~] base)  base:lib:grubbery
  =/  =grub:g  (need (~(get of cone) (welp /bin/base base)))
  ?>  ?=(%stem -.kind.grub)
  ?>  =([%& ~] tidy.kind.grub)
  =/  res  (mule |.(!<(base:g data.grub)))
  ?:  ?=(%& -.res)
    p.res
  ~|("base {(spud base)} failed to compile" !!)
::
::
++  get-stem
  |=  stem=path
  ^-  stem:g
  ?:  ?=([%bin ~] stem)  stem:bin:grubbery
  =/  =grub:g  (need (~(get of cone) (welp /bin/stem stem)))
  ?>  ?=(%stem -.kind.grub)
  ?>  =([%& ~] tidy.kind.grub)
  =/  res  (mule |.(!<(stem:g data.grub)))
  ?:  ?=(%& -.res)
    p.res
  ~|("stem {(spud stem)} failed to compile" !!)
::
++  get-stud
  |=  =stud:g
  ^-  mold
  ?:  ?=([%sig ~] stud)  ,~
  ?:  ?=([%lib ~] stud)
    ,[@t (each [(list (pair term path)) hoon] tang)]
  ?:  ?=([%bin ~] stud)  noun
  =/  =grub:g
    ~|  "{(spud stud)}: stud not found"
    (need (~(get of cone) (welp /bin/stud stud)))
  ?>  ?=(%stem -.kind.grub)
  ?>  =([%& ~] tidy.kind.grub)
  :: only useful for clamming
  =/  res  (mule |.(!<(mold data.grub)))
  ?:  ?=(%& -.res)
    p.res
  ~|("stud {(spud stud)} failed to compile" !!)
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
  ?~  nod=(~(get of cone) here)
    %& :: non-existent grubs aren't a cycle
  ?:  ?=(%base -.kind.u.nod)
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
  =/  =grub:g  (need (~(get of cone) here))
  ?>  ?=(%stem -.kind.grub)
  |-
  ?~  sour
    this(cone (~(put of cone) here grub))
  ?.  (~(has of cone) i.sour)
    ~|([here+here source+i.sour] !!)
  =/  =tack:g  (need (~(get of trac) i.sour))
  =.  sinx.tack  (~(put in sinx.tack) here)
  =.  trac  (~(put of trac) i.sour tack)
  =.  sour.kind.grub  (~(put by sour.kind.grub) i.sour step.last.tack)
  $(sour t.sour)
::
++  dirty
  |=  here=path
  ^-  [(set path) cone:g]
  =/  =grub:g  (need (~(get of cone) here))
  =?  cone  ?=(%stem -.kind.grub)
    %+  ~(put of cone)  here
    grub(tidy.kind [| ~])
  =/  =tack:g  (need (~(get of trac) here))
  ?:  =(0 ~(wyt in sinx.tack))
    [(sy ~[here]) cone]
  =/  sinx=(list path)  ~(tap in sinx.tack)
  =|  edge=(set path)
  |-
  ?~  sinx
    [edge cone]
  =^  e  cone
    (dirty i.sinx)
  $(sinx t.sinx, edge (~(uni in edge) e))
::
++  make-deps
  |=  [here=path sour=(set path)]
  ^-  (map path vase)
  %-  ~(gas by *(map path vase))
  %+  turn  ~(tap in sour)
  |=  =path
  :: ?>  (sand-bubble here ~ path %peek)
  :-  path
  data:(need (~(get of cone) path))
::
++  tidy-stem
  |=  [=give:g here=path]
  ^+  this
  ~&  >>  %tidy-stem
  =/  res=(each _this tang)
    (mule |.((tidy here)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %grubbery %$ ^] from.give)
    ?:(?=(%| -.res) !! this)
  %^    ingest
      [(scot %p our.bowl) /gall/grubbery]
    t.t.t.t.from.give
  [~ %tidy wire.give err]
::
++  tidy
  |=  here=path
  ^+  this
  :: deal with non-existent source later
  ::
  ?~  nod=(~(get of cone) here)  this
  =/  =grub:g  u.nod
  :: bases are always tidy
  ::
  ?.  ?=(%stem -.kind.grub)  this
  :: tidy stems stay tidy
  ::
  ?:  flag.tidy.kind.grub  this
  =/  sour=(list (pair path @da))  ~(tap by sour.kind.grub)
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
      =.  grub  grub(tidy.kind [%& ~])
      =.  cone  (~(put of cone) here grub)
      this
    ?~  dep=(~(get of cone) p.i.sour)
      :: If the dep doesn't exist, update boom
      ::
      =/  =tang
        :~  leaf+"stem boom"
            leaf+(spud here)
            leaf+"{(spud p.i.sour)} has no grub"
        ==
      :: %-  (slog tang)
      =.  boom.tidy.kind.grub  [~ tang]
      =.  cone  (~(put of cone) here grub)
      this
    ?:  &(?=(%stem -.kind.u.dep) ?=(^ boom.tidy.kind.u.dep))
      :: If a source is a stem and has a boom (crashed on recompute),
      :: we inherit its boom
      ::
      =.  boom.tidy.kind.grub  boom.tidy.kind.u.dep
      =.  cone  (~(put of cone) here grub)
      this
    :: if the source step.last has changed, we are not tidy
    ::
    ?:  (lth q.i.sour step.last:(need (~(get of trac) p.i.sour)))
      $(tidy %|)
    $(sour t.sour)
  :: If we are not tidy because our sources have updated
  :: recompute here
  ::
  =/  res=(each [darts=(list dart:g) data=vase] tang)
    %-  mule  |.
    =/  =stem:g  (get-stem stem.kind.grub)
    =/  deps=(map path vase)
      (make-deps here ~(key by sour.kind.grub)) :: sandboxed deps
    (stem [now our eny here deps]:[bowl .])
  ?-    -.res
      %|
    =/  =tang  [leaf+"stem boom" leaf+(spud here) p.res]
    :: %-  (slog tang)
    =.  grub  grub(tidy.kind [%| ~ tang])
    =.  cone  (~(put of cone) here grub)
    this
      %&
    =?  this  !=(data.grub data.p.res)  (next-tack here)
    =.  grub  grub(data data.p.res, tidy.kind [%& ~])
    =.  cone  (~(put of cone) here grub)
    %-  emit-bolts
    %+  turn  darts.p.res
    |=  =dart:g
    ?-    -.dart
        %sysc
      ~|  "ERROR: {(spud here)} has no system access"
      ?>  (sand-bubble here ~)
      [here dart]
      ::
        %scry
      ~|  "ERROR: {(spud here)} has no system access"
      ?>  (sand-bubble here ~)
      [here dart]
      ::
        %grub
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
  =^  e  cone  (dirty here)
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
  ~|  "deleting sources of {(spud here)} failed"
  =/  =grub:g  (need (~(get of cone) here))
  ?>  ?=(%stem -.kind.grub)
  =/  sour=(list path)  (turn ~(tap in sour.kind.grub) head)
  |-
  ?~  sour
    this
  =/  =tack:g    (need (~(get of trac) i.sour))
  =.  sinx.tack  (~(del in sinx.tack) here)
  ?:  &(=(~ sinx.tack) !(~(has of cone) i.sour))
    $(sour t.sour, trac (~(del of trac) i.sour))
  $(sour t.sour, trac (~(put of trac) i.sour tack))
::
++  do-oust
  |=  here=path
  ^+  this
  ?~  grub=(~(get of cone) here)
    ~&  >>  "nothing to oust at {(spud here)}"
    this
  =.  this
    ?-  -.kind.u.grub
      %base  (kill here)
      %stem  (del-sources here)
    ==
  =.  cone  (~(del of cone) here)
  (next-tack here)
::
++  oust-grub
  |=  [=give:g here=path]
  ^+  this
  =/  res=(each _this tang)  (mule |.((do-oust here)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %grubbery %$ ^] from.give)
    ?:(?=(%& -.res) this (mean p.res))
  %^    ingest
      [(scot %p our.bowl) /gall/grubbery]
    t.t.t.t.from.give
  [~ %gone wire.give err]
::
++  new-base
  |=  [here=path stud=path base=path data=(unit vase)]
  ^+  this
  ~|  "making base {(spud here)} failed"
  ?<  (~(has of cone) here)
  ?<  ?=([%bin *] here) :: bases not allowed in /bin
  ?<  ?=([%acl *] here) :: bases not allowed in /acl
  ?<  ?=([%san *] here) :: bases not allowed in /san
  =/  =mold  (get-stud stud)
  =/  data=vase  (fall data *vase)
  =/  =grub:g  [data stud [%base base ~]]
  =.  cone  (~(put of cone) here grub)
  =.  this  (next-tack here)
  (dirty-and-tidy here)
::
++  make-base
  |=  [=give:g here=path stud=path base=path data=(unit vase)]
  ^+  this
  =/  res=(each _this tang)  (mule |.((new-base here stud base data)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %grubbery %$ ^] from.give)
    ?:(?=(%& -.res) this (mean p.res))
  %^    ingest
      [(scot %p our.bowl) /gall/grubbery]
    t.t.t.t.from.give
  [~ %made wire.give err]
::
++  new-stem
  |=  [here=path stud=path stem=path sour=(set path)]
  ^+  this
  ~|  "making stem {(spud here)} failed"
  ?<  (~(has of cone) here)
  ?<  ?=([%lib *] here) :: stems not allowed in /lib
  =/  =grub:g  [!>(~) stud [%stem stem [| ~] ~]]
  =.  cone     (~(put of cone) here grub)
  =.  this     (add-sources here sour)
  =.  this     (next-tack here)
  (dirty-and-tidy here)
::
++  make-stem
  |=  [=give:g here=path stud=path stem=path sour=(set path)]
  ^+  this
  =/  res=(each _this tang)  (mule |.((new-stem here stud stem sour)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %grubbery %$ ^] from.give)
    ?:(?=(%& -.res) this (mean p.res))
  %^    ingest
      [(scot %p our.bowl) /gall/grubbery]
    t.t.t.t.from.give
  [~ %made wire.give err]
::
++  take-peek
  |=  [here=path =wire pat=path]
  ^+  this
  =/  from=path  [(scot %p our.bowl) /gall/grubbery]
  (ingest from here ~ %peek wire pat (~(dip of cone) pat))
::
++  take-scry
  |=  [here=path =wire =mold pat=path]
  ^+  this
  =/  from=path  [(scot %p our.bowl) /gall/grubbery]
  =;  =vase
    (ingest from here ~ %scry wire pat vase)
  ?>  ?=(^ pat)
  ?>  ?=(^ t.pat)
  !>(.^(mold i.pat (scot %p our.bowl) i.t.pat (scot %da now.bowl) t.t.pat))
::
++  kill
  |=  here=path
  ^+  this
  ~|  "killing {(spud here)} failed"
  =/  =tack:g  (need (~(get of trac) here))
  =/  =grub:g  (need (~(get of cone) here))
  ?>  ?=(%base -.kind.grub)
  ?~  proc.kind.grub
    ~&  >>  "no processes to kill for {(spud here)}"
    this
  =.  cone  (~(put of cone) here grub(proc.kind ~))
  |-
  =.  this  (give-poke-result here %| %killed [leaf+(spud here) ~])
  =^  poke  trac
    (stage-next-poke here)
  ?^(poke $ this) :: repeat until no poke staged
::
++  kill-base
  |=  [=give:g here=path]
  ^+  this
  =/  res=(each _this tang)  (mule |.((kill here)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %grubbery %$ ^] from.give)
    ?:(?=(%| -.res) !! this)
  %^    ingest
      [(scot %p our.bowl) /gall/grubbery]
    t.t.t.t.from.give
  [~ %dead wire.give err]
::
++  bump-base
  |=  [here=path =give:g =pail:g]
  ^+  this
  =/  res=(each _this tang)
    (mule |.((ingest from.give here ~ %bump pail)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %grubbery %$ ^] from.give)
    ?:(?=(%| -.res) !! this)
  %^    ingest
      [(scot %p our.bowl) /gall/grubbery]
    t.t.t.t.from.give
  [~ %base wire.give %bump err]
:: TODO: handle outgoing keens
::
++  clean
  |=  [here=path last=@da]
  ^+  this
  %-  emit-cards
  %+  murn  ~(tap by wex.bowl)
  |=  [[=wire =ship =term] *]
  ^-  (unit card)
  ?.  ?=([%base @ *] wire)
    ~
  =/  [h=path l=@da *]  (unwrap-wire wire)
  ?.  &(=(h here) =(l last))
    ~
  [~ %pass wire %agent [ship term] %leave ~]
::
++  poke-base
  |=  [here=path =poke:g]
  ^+  this
  ~&  %take-poke
  =/  =tack:g  (need (~(get of trac) here))
  =.  trac  (~(put of trac) here tack(line (~(put to line.tack) poke)))
  (run-next-poke here)
::
++  stage-next-poke
  |=  here=path
  ^-  [(unit poke:g) trac:g]
  =/  =tack:g  (need (~(get of trac) here))
  ?>  =(~ give.tack)
  ?:  =(~ line.tack)
    [~ trac] :: no more pokes to run
  =/  [=poke:g line=(qeu poke:g)]  ~(get to line.tack)
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
  =/  =grub:g  (need (~(get of cone) here))
  =^  poke  trac
    (stage-next-poke here)
  ?~  poke 
    this
  ?>  ?=(%base -.kind.grub)
  =/  =bowl:base:g  [now our eny ~ ~ from.give.u.poke here]:[bowl .]
  =/  build=(each proc:base:g tang)
    (mule |.(((get-base base.kind.grub) bowl pail.u.poke)))
  ?:  ?=(%& -.build)
    =.  cone  (~(put of cone) here grub(proc.kind `p.build))
    =/  from=path  [(scot %p our.bowl) /gall/grubbery]
    (ingest from here ~) :: start
  :: %-  (slog [leaf+"build-fail" leaf+(spud here) p.build])
  =.  this  (give-poke-result here %| %build-fail p.build)
  (run-next-poke here)
::
++  give-poke-result
  |=  [here=path res=(each pail:g goof)]
  ^+  this
  ~&  %giving-poke-result
  ~&  here+here
  =/  =grub:g  (need (~(get of cone) here))
  ?>  ?=(%base -.kind.grub)
  =/  =tack:g  (need (~(get of trac) here))
  ?>  ?=(^ give.tack)
  =.  cone  (~(put of cone) here grub(proc.kind ~))
  =.  trac  (~(put of trac) here tack(give ~))
  =.  this  (clean here poke.last.tack)
  ?:  ?=([@ %clay ~] from.u.give.tack) :: +on-load
    ?:  ?=(%& -.res)
      this
    ~&  >>>  mote.p.res
    (mean tang.p.res)
  ?:  ?=([@ %gall %grubbery %$ ^] from.u.give.tack)
    %^    ingest
        [(scot %p our.bowl) /gall/grubbery]
      t.t.t.t.from.u.give.tack
    [~ %base wire.u.give.tack %poke res]
  ?:  ?=([@ %gall @ ~] from.u.give.tack)
    =/  =wire  (weld /poke/[i.from] wire):[u.give.tack .]
    %-  emit-cards
    :~  [%give %fact ~[wire] sign-base+!>([%poke res])]
        [%give %kick ~[wire] ~]
    ==
  ?>  ?=([@ %eyre @ @ ~] from.u.give.tack)
  =/  src=@p       (slav %p i.t.t.from.u.give.tack)
  =/  eyre-id=@ta  i.t.t.t.from.u.give.tack
  %-  emit-cards
  %+  give-simple-payload:app:server
    eyre-id
  ?:  ?=(%| -.res)
    ~&  >>>  mote.p.res
    %-  (slog tang.p.res)
    %^    internal-server-error:io
        =(src our.bowl)
      "Base Grub Crashed"
    [leaf+(trip mote.p.res) tang.p.res]
  ?.  ?=([%simple-payload ~] p.p.res)
    %^    internal-server-error:io
        =(src our.bowl)
      "Bad Grub Response"
    [leaf+"bad grub response" leaf+(spud p.p.res) ~]
  =/  pay=(each simple-payload:http tang)
    (mule |.(!<(simple-payload:http q.p.res)))
  ?:  ?=(%& -.pay)
    p.pay
  %^    internal-server-error:io
      =(src our.bowl)
    "Bad Grub Response"
  p.pay
::
++  make-bowl
  |=  [from=path here=path]
  ^-  bowl:base:g
  =/  last=@da  poke.last:(need (~(get of trac) here))
  =.  wex.bowl
    %-  ~(gas by *boat:gall)
    %+  murn  ~(tap by wex.bowl)
    |=  [[=wire =ship =term] acked=? pat=path]
    ?.  ?=([%base @ *] wire)
      ~
    =/  [h=path l=@da w=path]  (unwrap-wire wire)
    ?.  &(=(h here) =(l last))
      ~
    [~ [w ship term] acked pat]
  =.  sup.bowl
    %-  ~(gas by *bitt:gall)
    %+  murn  ~(tap by sup.bowl)
    |=  [=duct =ship pat=path]
    ?.  ?=([%base @ *] pat)
      ~
    =/  [h=path l=@da p=path]  (unwrap-wire pat)
    ?.  &(=(h here) =(l last))
      ~
    [~ duct ship p]
  [now our eny wex sup from here]:[bowl .]
:: TODO: immediately give response if a card is not allowed
::       by sandboxing
::
++  ingest
  |=  [from=path here=path in=(unit intake:base:g)]
  ^+  this
  ~&  %ingest
  =/  =grub:g  (need (~(get of cone) here))
  ?>  ?=(%base -.kind.grub)
  ?.  ?=(^ proc.kind.grub)
    ~&  >>  "process not running at {(spud here)}"
    :: TODO: Add process id (last) to the wire (in transit)
    ::       And drop inputs if that process is dead
    ::
    this
  =/  m  (charm:base:g ,pail:g)
  =/  =bowl:base:g  (make-bowl from here)
  =/  res=(each [[(list dart:g) vase result:eval:m] proc:base:g] tang)
    (mule |.((take:eval:m u.proc.kind.grub bowl data.grub in)))
  =/  [[darts=(list dart:g) data=vase =result:eval:m] =proc:base:g]
    ?-  -.res
      %&  p.res
      %|  [[~ data.grub [%fail %crash [leaf+(spud here) p.res]]] u.proc.kind.grub]
    ==
  ::
  =.  this
    %-  emit-bolts
    %+  turn  darts
    |=  =dart:g
    ?-    -.dart
        %sysc
      ~|  "ERROR: {(spud here)} has no system access"
      ?>  (sand-bubble here ~)
      [here dart]
      ::
        %scry
      ~|  "ERROR: {(spud here)} has no system access"
      ?>  (sand-bubble here ~)
      [here dart]
      ::
        %grub
      ~|  "ERROR: {(spud here)} has no {(trip -.load.dart)} access to {(spud path.dart)}"
      ?>  (sand-bubble here ~ [path -.load]:dart)
      [here dart]
    ==
  ::
  =/  tick=?  !=(data data.grub)
  =?  this  tick  (next-tack here)
  =.  grub  grub(data data)
  =.  cone
    ?.  ?=(%next -.result)
      (~(put of cone) here grub(data data, proc.kind ~))
    (~(put of cone) here grub(data data, proc.kind `proc))
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
    /base/(scot %ud (lent here))
    here  /(scot %da last)  wire
  ==
::
++  unwrap-wire
  |=  =wire
  ^-  [path @da ^wire]
  ?>  ?=([%base @ *] wire)
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
    (emit-card [%pass (wrap-wire here last p.card) q.card])
  ==
::
++  take-arvo
  |=  [wir=wire sign=sign-arvo]
  ^+  this
  =/  [here=path last=@da =wire]  (unwrap-wire wir)
  =/  curr=@da  poke.last:(need (~(get of trac) here))
  ?.  =(last curr)
    ~&  >>  "discarding message for old base process"
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
    ~&  >>  "discarding message for old base process"
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
    ~&  >>  "discarding message for old base process"
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
    ~&  >>  "discarding message for old base process"
    this
  (ingest from here ~ %leave pan)
--
