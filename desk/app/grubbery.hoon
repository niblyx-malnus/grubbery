/-  g=grubbery
/+  grubbery, io=grubberyio, server, dbug, verb, default-agent
/=  x-  /mar/sign-base
|%
+$  card     card:agent:gall
+$  state-0
  $:  %0
      =cone:g
      =trac:g
      =sand:g
      =bindings:g
      =history:g
  ==
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
  ?+    pole  [~ ~]
      [%x %history since=@ta ~]
    =/  since=@da  (slav %da since.pole)
    ``noun+!>((tap:hon:g (lot:hon:g history ~ `since)))
  ==
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
        ?:  ?=(%base -.u.get)
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
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(oust-grub:hc give here)
    [cards this]
    ::
      %cull-cone
    =+  !<(here=path vase)
    ~&  here+here
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(cull-cone:hc give here)
    [cards this]
    ::
      %make-base
    =+  !<([here=path base=path data=(unit ^vase)] vase)
    ~&  here+here
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(make-base:hc give here base data)
    [cards this]
    ::
      %make-stem
    =+  !<([here=path stem=path sour=(set path)] vase)
    ~&  here+here
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(make-stem:hc give here stem sour)
    [cards this]
    ::
      %poke-base
    =+  !<([=wire here=path =pail:g] vase)
    ~&  here+here
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] wire]
    =^  cards  state
      abet:(poke-base:hc here give pail)
    [cards this]
    ::
      %bump-base
    =+  !<([here=path =pail:g] vase)
    ~&  here+here
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(bump-base:hc here give pail)
    [cards this]
    ::
      %kill-base
    =+  !<(here=path vase)
    ~&  here+here
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(kill-base:hc give here)
    [cards this]
    ::
      %edit-perm
    =+  !<([here=path perm=(unit perm:g)] vase)
    ~&  here+here
    =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
    =^  cards  state
      abet:(edit-perm:hc give here perm)
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
      %perk
    $(this (give-perk here [wire pail]:dart))
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
        %base  $(this (make-base give [path [base data]:make.load]:dart))
        %stem  $(this (make-stem give [path [stem sour]:make.load]:dart))
      ==
      ::
        %oust
      $(this (oust-grub [from wire] path):[dart .])
      ::
        %cull
      $(this (cull-cone [from wire] path):[dart .])
      ::
        %sand
      $(this (edit-perm [from wire] path perm.load):[dart .])
      ::
        %kill
      $(this (kill-base [from wire] path):[dart .])
    ==
  ==
::
++  boot
  ^+  this
  =/  =give:g  [[(scot %p src.bowl) sap.bowl] /]
  =.  this  (oust-grub give /boot)
  =.  this  (make-base give /boot /boot ~)
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
::
++  get-base-code
  |=  base=path
  ^-  base:g
  ?:  ?=([%boot ~] base)  boot:grubbery
  ?:  ?=([%lib ~] base)  base:lib:grubbery
  ?:  ?=([%bin ~] base)  base:bin:grubbery
  =/  =grub:g
    ~|  "{(spud base)}: base not found"
    (need (~(get of cone) (welp /bin/base base)))
  ?>  ?=(%stem -.grub)
  ?>  tidy.grub
  =/  res  (mule |.(!<([* b=base:g] (grab-data:io grub))))
  ?:  ?=(%& -.res)
    b.p.res
  ~|("base {(spud base)} failed to compile" !!)
::
++  get-base-stud
  |=  base=path
  ^-  stud:g
  ?:  ?=([%boot ~] base)  /sig
  ?:  ?=([%lib ~] base)  /lib
  ?:  ?=([%bin ~] base)  /bin
  =/  =grub:g
    ~|  "{(spud base)}: base not found"
    (need (~(get of cone) (welp /bin/base base)))
  ?>  ?=(%stem -.grub)
  ?>  tidy.grub
  =/  res  (mule |.(!<([=stud:g *] (grab-data:io grub))))
  ?:  ?=(%& -.res)
    stud.p.res
  ~|("base {(spud base)} failed to compile" !!)
::
++  get-stem-code
  |=  stem=path
  ^-  stem:g
  ?:  ?=([%bin ~] stem)  stem:bin:grubbery
  =/  =grub:g  
    ~|  "{(spud stem)}: stem not found"
    (need (~(get of cone) (welp /bin/stem stem)))
  ?>  ?=(%stem -.grub)
  ?>  tidy.grub
  =/  res  (mule |.(!<([* s=stem:g] (grab-data:io grub))))
  ?:  ?=(%& -.res)
    s.p.res
  ~|("stem {(spud stem)} failed to compile" !!)
::
++  get-stem-stud
  |=  stem=path
  ^-  stud:g
  ?:  ?=([%bin ~] stem)  /bin
  =/  =grub:g  
    ~|  "{(spud stem)}: stem not found"
    (need (~(get of cone) (welp /bin/stem stem)))
  ?>  ?=(%stem -.grub)
  ?>  tidy.grub
  =/  res  (mule |.(!<([=stud:g *] (grab-data:io grub))))
  ?:  ?=(%& -.res)
    stud.p.res
  ~|("stem {(spud stem)} failed to compile" !!)
:: only useful for clamming
::
++  get-stud
  |=  =stud:g
  ^-  vase
  ?:  ?=([%sig ~] stud)  !>(,~)
  ?:  ?=([%lib ~] stud)
    !>(,[@t (each [(list (pair term path)) hoon] tang)])
  ?:  ?=([%bin ~] stud)  !>(noun)
  =/  =grub:g
    ~|  "{(spud stud)}: stud not found"
    (need (~(get of cone) (welp /bin/stud stud)))
  ?>  ?=(%stem -.grub)
  ?>  tidy.grub
  (grab-data:io grub)
::
++  bunt-stud
  |=  =stud:g
  ^-  vase
  =/  func=vase  (get-stud stud)
  (slam func (slot 6 func))
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
  ?:  ?=(%base -.u.nod)
    %&
  =/  sour=(list path)  ~(tap in ~(key by sour.u.nod))
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
  ?>  ?=(%stem -.grub)
  |-
  ?~  sour
    this(cone (~(put of cone) here grub))
  =/  tac=(unit tack:g)  (~(get of trac) i.sour)
  =?  this  ?=(~ tac)  (next-tack i.sour)
  =/  =tack:g  (need (~(get of trac) i.sour))
  =.  sinx.tack  (~(put in sinx.tack) here)
  =.  trac  (~(put of trac) i.sour tack)
  =.  sour.grub  (~(put by sour.grub) i.sour `@da`0) :: 0 forces recompute
  $(sour t.sour)
::
++  allowed
  =|  prefix=(unit path)
  |=  [here=path =dart:g]
  ^-  (each (unit path) ~)
  =/  res  (allowed:grubbery dart (~(get of sand) here))
  ?-    -.res
    %|  [%| ~]
      %&
    =?  prefix  &(?=(~ prefix) ?=(^ p.res))  p.res
    ?:  =(~ here)
      [%& prefix]
    $(here (snip here))
  ==
::
++  handle-base-emits
  |=  [here=path darts=(list dart:g)]
  ^+  this
  ?~  darts
    this
  =.  this  (handle-base-emit here i.darts)
  $(darts t.darts)
::
++  handle-base-emit
  |=  [here=path =dart:g]
  ^+  this
  =/  res  (allowed here dart)
  ?-    -.res
      %|
    ~&  >>>  "vetoing illegal dart from {(spud here)}"
    %^    ingest
        [[(scot %p our.bowl) /gall/grubbery] /]
      here
    [~ %veto dart]
    ::
      %&
    ?~  p.res                                 (emit-bolt here dart)
    ?^  (decap:grubbery u.p.res here)         (emit-bolt here dart)
    ?.  ?=(%grub -.dart)                      (emit-bolt here dart)
    ?.  ?=(?(%poke %bump %make) -.load.dart)  (emit-bolt here dart)
    ?-    -.load.dart
        %poke
      =/  res  (mule |.((get-stud p.pail.load.dart)))
      ?:  ?=(%| -.res)
        %^    ingest
            [[(scot %p our.bowl) /gall/grubbery] /]
          here
        [~ %base wire.dart %poke ~ %poke-stud-fail p.res]
      =/  res  (mule |.((slam p.res q.pail.load.dart)))
      ?:  ?=(%| -.res)
        %^    ingest
            [[(scot %p our.bowl) /gall/grubbery] /]
          here
        [~ %base wire.dart %poke ~ %poke-clam-fail p.res]
      (emit-bolt here dart(q.pail.load p.res))
      ::
        %bump
      =/  res  (mule |.((get-stud p.pail.load.dart)))
      ?:  ?=(%| -.res)
        %^    ingest
            [[(scot %p our.bowl) /gall/grubbery] /]
          here
        [~ %base wire.dart %bump ~ leaf+"bump-stud-fail" p.res]
      =/  res  (mule |.((slam p.res q.pail.load.dart)))
      ?:  ?=(%| -.res)
        %^    ingest
            [[(scot %p our.bowl) /gall/grubbery] /]
          here
        [~ %base wire.dart %bump ~ leaf+"bump-clam-fail" p.res]
      (emit-bolt here dart(q.pail.load p.res))
      ::
        %make
      ?:  ?=(%stem -.make.load.dart)  (emit-bolt here dart)
      ?~  data.make.load.dart         (emit-bolt here dart)
      =/  res
        (mule |.((get-stud (get-base-stud base.make.load.dart))))
      ?:  ?=(%| -.res)
        %^    ingest
            [[(scot %p our.bowl) /gall/grubbery] /]
          here
        [~ %made wire.dart ~ %make-stud-fail p.res]
      =/  res  (mule |.((slam p.res u.data.make.load.dart)))
      ?:  ?=(%| -.res)
        %^    ingest
            [[(scot %p our.bowl) /gall/grubbery] /]
          here
        [~ %made wire.dart ~ %make-clam-fail p.res]
      (emit-bolt here dart(data.make.load [~ p.res]))
    ==
  ==
::
++  dirty
  |=  here=path
  ^-  [(set path) _this]
  :: ~&  >>  "dirtying {(spud here)}"
  =/  =grub:g  (need (~(get of cone) here))
  =/  =tack:g  (need (~(get of trac) here))
  ?:  &(?=(%stem -.grub) !tidy.grub)
    [~ this]
  =?  cone  ?=(%stem -.grub)
    (~(put of cone) here grub(tidy |))
  ?:  =(0 ~(wyt in sinx.tack))
    ?:  ?=(%base -.grub)
      [~ this]
    [(sy ~[here]) this]
  =/  sinx=(list path)  ~(tap in sinx.tack)
  =|  edge=(set path)
  |-
  ?~  sinx
    [edge this]
  =^  e  this  (dirty i.sinx)
  %=  $
    sinx   t.sinx
    edge   (~(uni in edge) e)
  ==
::
++  tidy
  |=  here=path
  ^+  this
  :: ~&  >>  "tidying {(spud here)}"
  ?~  grub=(~(get of cone) here)
    :: ~&  >>  "{(spud here)} has no data"
    this
  ?:  ?=(%base -.u.grub)
    :: ~&  >>  "{(spud here)} is a base and thus tidy"
    this
  ?:  tidy.u.grub
    :: ~&  >>  "{(spud here)} is already tidy"
    this
  =/  sour=(list (pair path @da))  ~(tap by sour.u.grub)
  |-
  ?~  sour
    (recompute-stem here u.grub)
  $(sour t.sour, this (tidy p.i.sour))
::
++  make-deps
  |=  [here=path sour=(set path)]
  ^-  (map path (each vase tang))
  %-  ~(gas by *(map path (each vase tang)))
  %+  turn  ~(tap in sour)
  |=  =path
  :-  path
  ?~  grub=(~(get of cone) path)
    |+[leaf+"no grub" leaf+(spud here) ~]
  (grab-data-soft:io u.grub)
::
++  make-sour
  |=  sour=(set path)
  ^-  (map path @da)
  %-  ~(gas by *(map path @da))
  %+  turn  ~(tap in sour)
  |=  =path
  :-  path
  step.last:(need (~(get of trac) path))
::
++  recompute-stem
  |=  [here=path =grub:g]
  ^+  this
  :: ~&  >>  "recompute stem"
  ?>  ?=(%stem -.grub)
  =/  new-sour=(map path @da)  (make-sour ~(key by sour.grub))
  ?:  =(new-sour sour.grub)
    :: ~&  >>  "{(spud here)} hasn't changed on recompute"
    =.  grub  grub(tidy %&)
    this(cone (~(put of cone) here grub))
  =/  res=(each [darts=(list dart:g) data=vase] tang)
    %-  mule  |.
    =/  =stem:g  (get-stem-code stem.grub)
    =/  deps=(map path (each vase tang))
      (make-deps here ~(key by sour.grub)) :: sandboxed deps
    (stem [now our eny here deps]:[bowl .])
  ?-    -.res
      %|
    :: ~&  >>>  "{(spud here)} crashed on recompute"
    =/  =tang  [leaf+"stem boom" leaf+(spud here) p.res]
    =?  this  !=(data.grub |+tang)  (next-tack here)
    :: %-  (slog tang)
    =.  grub  grub(tidy %&, data |+tang)
    this(cone (~(put of cone) here grub))
    ::
      %&
    :: ~&  >  "{(spud here)} successfully recomputed"
    =?  this  !=(data.grub &+data.p.res)  (next-tack here)
    =.  grub
      grub(data &+data.p.res, sour new-sour, tidy %&)
    =.  cone  (~(put of cone) here grub)
    =/  bolts=(list bolt:g)  (turn darts.p.res (lead here))
    %-  emit-bolts
    %+  murn  bolts
    |=  [here=path =dart:g]
    =/  res  (allowed here dart)
    ?:  ?=(%| (allowed here dart))
      :: ~&  >>  "ignoring sandboxed dart from {(spud here)}"
      ~
    [~ here dart]
  ==
::
++  dirty-and-tidy
  |=  here=path
  ^+  this
  :: ~&  >>  "dirty-and-tidy {(spud here)}"
  =^  e  this  (dirty here)
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
  ?>  ?=(%stem -.grub)
  =/  sour=(list path)  (turn ~(tap in sour.grub) head)
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
    this
  =.  this
    ?-  -.u.grub
      %base  (kill here)
      %stem  (del-sources here)
    ==
  =.  cone  (~(del of cone) here)
  (next-tack here)
::
++  do-cull
  |=  here=path
  ^+  this
  =/  hone=cone:g  (~(dip of cone) here)
  ?:  =(~ dir.hone)
    =.  this  (do-oust here)
    this(cone (~(lop of cone) here))
  =/  next=(list @ta)  ~(tap in ~(key by dir.hone))
  |-
  ?~  next
    this
  =.  this  (do-cull (snoc here i.next))
  $(next t.next)
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
      [[(scot %p our.bowl) /gall/grubbery] /]
    t.t.t.t.from.give
  [~ %gone wire.give err]
::
++  cull-cone
  |=  [=give:g here=path]
  ^+  this
  =/  res=(each _this tang)  (mule |.((do-cull here)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %grubbery %$ ^] from.give)
    ?:(?=(%& -.res) this (mean p.res))
  %^    ingest
      [[(scot %p our.bowl) /gall/grubbery] /]
    t.t.t.t.from.give
  [~ %cull wire.give err]
::
++  put-sand
  |=  [here=path perm=(unit perm:g)]
  ^+  this
  ?>  ?=(^ here) :: root should always have system access
  ?~  perm
    this(sand (~(del of sand) here))
  =.  u.perm  (clean-perm:grubbery here u.perm)
  this(sand (~(put of sand) here u.perm))
::
++  edit-perm
  |=  [=give:g here=path perm=(unit perm:g)]
  ^+  this
  =/  res=(each _this tang)  (mule |.((put-sand here perm)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %grubbery %$ ^] from.give)
    ?:(?=(%& -.res) this (mean p.res))
  %^    ingest
      [[(scot %p our.bowl) /gall/grubbery] /]
    t.t.t.t.from.give
  [~ %sand wire.give err]
::
++  new-base
  |=  [here=path base=path data=(unit vase)]
  ^+  this
  ~|  "making base {(spud here)} failed"
  ?<  (~(has of cone) here)
  =/  =stud:g  (get-base-stud base)
  =/  data=vase  (fall data (bunt-stud stud))
  =/  =grub:g  [%base data base ~]
  =.  cone  (~(put of cone) here grub)
  =.  this  (next-tack here)
  (dirty-and-tidy here)
::
++  make-base
  |=  [=give:g here=path base=path data=(unit vase)]
  ^+  this
  =/  res=(each _this tang)  (mule |.((new-base here base data)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %grubbery %$ ^] from.give)
    ?:(?=(%& -.res) this (mean p.res))
  %^    ingest
      [[(scot %p our.bowl) /gall/grubbery] /]
    t.t.t.t.from.give
  [~ %made wire.give err]
::
++  new-stem
  |=  [here=path stem=path sour=(set path)]
  ^+  this
  ~|  "making stem {(spud here)} failed"
  ?<  =(~ sour)
  ?<  (~(has of cone) here)
  ?<  ?=([%lib *] here) :: stems not allowed in /lib
  =/  =stud:g  (get-stem-stud stem)
  =/  =grub:g  [%stem |+[leaf+"new stem"]~ stem | ~]
  =.  cone     (~(put of cone) here grub)
  =.  this     (add-sources here sour)
  =.  this     (next-tack here)
  ~&  >>  "computing-new-stem {(spud here)}"
  =.  this     (recompute-stem here (need (~(get of cone) here)))
  (dirty-and-tidy here)
::
++  make-stem
  |=  [=give:g here=path stem=path sour=(set path)]
  ^+  this
  =/  res=(each _this tang)  (mule |.((new-stem here stem sour)))
  =/  err=(unit tang)  ?-(-.res %& ~, %| `p.res)
  =?  this  ?=(%& -.res)  p.res
  ?.  ?=([@ %gall %grubbery %$ ^] from.give)
    ?:(?=(%& -.res) this (mean p.res))
  %^    ingest
      [[(scot %p our.bowl) /gall/grubbery] /]
    t.t.t.t.from.give
  [~ %made wire.give err]
::
++  take-peek
  |=  [here=path =wire pat=path]
  ^+  this
  =/  from=path  [(scot %p our.bowl) /gall/grubbery]
  (ingest [from /] here ~ %peek wire pat (~(dip of cone) pat) (~(dip of sand) pat))
::
++  take-scry
  |=  [here=path =wire =mold pat=path]
  ^+  this
  =/  from=path  [(scot %p our.bowl) /gall/grubbery]
  =;  =vase
    (ingest [from /] here ~ %scry wire pat vase)
  ?>  ?=(^ pat)
  ?>  ?=(^ t.pat)
  !>(.^(mold i.pat (scot %p our.bowl) i.t.pat (scot %da now.bowl) t.t.pat))
::
++  on-load-kill
  |=  here=path
  ^+  this
  !!
::
++  kill
  |=  here=path
  ^+  this
  ~|  "killing {(spud here)} failed"
  =/  =tack:g  (need (~(get of trac) here))
  =/  =grub:g  (need (~(get of cone) here))
  ?>  ?=(%base -.grub)
  ?~  proc.grub
    ~&  >>  "no processes to kill for {(spud here)}"
    this
  =.  cone  (~(put of cone) here grub(proc ~))
  |-
  =.  this  (give-poke-ack here ~ %killed [leaf+(spud here) ~])
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
      [[(scot %p our.bowl) /gall/grubbery] /]
    t.t.t.t.from.give
  [~ %dead wire.give err]
::
++  bump-base
  |=  [here=path =give:g =pail:g]
  ^+  this
  (ingest give here ~ %bump pail)
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
  ?.  =(~ give.tack)
    this
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
  ?>  ?=(%base -.grub)
  =/  =bowl:base:g  [now our eny ~ ~ from.give.u.poke here]:[bowl .]
  =/  build=(each proc:base:g tang)
    (mule |.(((get-base-code base.grub) bowl pail.u.poke)))
  ?:  ?=(%& -.build)
    =.  cone  (~(put of cone) here grub(proc `[p.build ~ ~]))
    =/  from=path  [(scot %p our.bowl) /gall/grubbery]
    (ingest [from /] here ~) :: start
  :: %-  (slog [leaf+"build-fail" leaf+(spud here) p.build])
  =.  this  (give-poke-ack here ~ %build-fail p.build)
  (run-next-poke here)
::
++  give-perk
  |=  [here=path back=wire =pail:g]
  ^+  this
  ~&  %giving-perk
  ~&  here+here
  =/  =grub:g  (need (~(get of cone) here))
  ?>  ?=(%base -.grub)
  =/  =tack:g  (need (~(get of trac) here))
  ?>  ?=(^ give.tack)
  ?:  ?=([@ %gall %grubbery %$ ^] from.u.give.tack)
    %^    ingest
        [[(scot %p our.bowl) /gall/grubbery] back]
      t.t.t.t.from.u.give.tack
    [~ %perk wire.u.give.tack pail]
  ?:  ?=([@ %gall @ ~] from.u.give.tack)
    :: TODO: figure out remote poke/bump/perk etc
    !!
  ?>  ?=([@ %eyre @ @ ~] from.u.give.tack)
  =/  src=@p       (slav %p i.t.t.from.u.give.tack)
  =/  eyre-id=@ta  i.t.t.t.from.u.give.tack
  =/  =wire  /http-response/[eyre-id]
  =/  =cage  
    ?+    p.pail  !!
        [%http-response-data ~]
      http-response-data+q.pail
        [%http-response-header ~]
      http-response-header+q.pail
    ==
  =.  this  (emit-card %give %fact ~[wire] cage)
  %^    ingest
      [[(scot %p our.bowl) /gall/grubbery] /]
    here
  [~ %base back %perk ~]
::
++  give-poke-ack
  |=  [here=path res=(unit tang)]
  ^+  this
  ~&  %giving-poke-ack
  ~&  here+here
  =/  =grub:g  (need (~(get of cone) here))
  ?>  ?=(%base -.grub)
  =/  =tack:g  (need (~(get of trac) here))
  ?>  ?=(^ give.tack)
  =.  cone  (~(put of cone) here grub(proc ~))
  =.  trac  (~(put of trac) here tack(give ~))
  =.  this  (clean here poke.last.tack)
  ?:  ?=([@ %clay ~] from.u.give.tack) :: +on-load
    ?~(res this (mean u.res))
  ?:  ?=([@ %gall %grubbery %$ ^] from.u.give.tack)
    %^    ingest
        [[(scot %p our.bowl) /gall/grubbery] /]
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
  (emit-card %give %kick ~[/http-response/[eyre-id]] ~)
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
:: ack for perk or bump
::
++  give-poke-sign
  |=  [=give:g in=(unit intake:base:g) err=(unit tang)]
  ^+  this
  =/  giv=give:g  [[(scot %p our.bowl) /gall/grubbery] /]
  =/  here=path  from.give
  ?+  in  this
    [~ %bump *]  (ingest giv from.give ~ %base wire.give %bump err)
    [~ %perk *]  (ingest giv from.give ~ %base wire.give %perk err)
  ==
::
++  ingest
  |=  [=give:g here=path in=(unit intake:base:g)]
  ^+  this
  =/  =grub:g  (need (~(get of cone) here))
  ?>  ?=(%base -.grub)
  ?.  ?=(^ proc.grub)
    ~&  >>  "process not running at {(spud here)}"
    :: TODO: Add process id (last) to the wire (in transit)
    ::       And drop inputs if that process is dead
    ::
    this
  =/  m  (charm:base:g ,~)
  =/  =bowl:base:g  (make-bowl from.give here)
  =/  res=(each [[(list dart:g) vase result:eval:m] proc:base:g] tang)
    (mule |.((take:eval:m proc.u.proc.grub bowl data.grub in)))
  =/  [[darts=(list dart:g) data=vase =result:eval:m] =proc:base:g]
    ?-  -.res
      %&  p.res
      %|  [[~ data.grub [%fail leaf+"crash" [leaf+(spud here) p.res]]] proc.u.proc.grub]
    ==
  ::
  =/  tick=?  !=(data data.grub)
  =?  this  tick  (next-tack here)
  =.  data.grub  data
  =.  cone
    ?:  ?=(%wait -.result)
      %+  ~(put of cone)  here
      grub(proc [~ proc [next skip]:u.proc.grub])
    ?:  ?=(%skip -.result)
      %+  ~(put of cone)  here
      =/  skip  (~(put to skip.u.proc.grub) [give in])
      grub(proc [~ proc next.u.proc.grub skip])
    ?:  ?=(?(%fail %done) -.result)
      (~(put of cone) here grub(proc ~))
    ?>  ?=(%cont -.result)
    %+  ~(put of cone)  here
    =/  next  
      %-  ~(gas to next.u.proc.grub)
      ~(tap to skip.u.proc.grub)
    grub(proc [~ proc next ~])
  ::
  =?  this  tick  (dirty-and-tidy here)
  ::
  =.  this  (give-poke-sign give in ?-(-.res %& ~, %| `p.res))
  =.  this  (handle-base-emits here darts)
  ::
  ?:  ?=(?(%wait %skip) -.result)
    ?:  =(~ next.u.proc.grub)
      this
    =/  =take:base:g  (need ~(top to next.u.proc.grub))
    =/  =grub:g  (need (~(get of cone) here))
    ?>  ?=(%base -.grub)
    ?>  ?=(^ proc.grub)
    =.  cone
      %+  ~(put of cone)  here
      =/  next  ~(nap to next.u.proc.grub)
      grub(proc [~ proc next skip.u.proc.grub])
    (ingest give.take here in.take)
  ?:  ?=(%cont -.result)
    =/  from=path  [(scot %p our.bowl) /gall/grubbery]
    (ingest [from /] here ~) :: continue
  ?>  ?=(?(%fail %done) -.result)
  =.  this
    %+  give-poke-ack
      here
    ?-  -.result
      %done  ~
      %fail  [~ err.result]
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
  (ingest [from /] here ~ %arvo wire sign)
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
  (ingest [from /] here ~ %agent wire sign)
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
  (ingest [from /] here ~ %watch pan)
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
  (ingest [from /] here ~ %leave pan)
--
