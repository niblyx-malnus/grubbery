/-  *grubbery
/+  server
=,  base
|%
++  get-ship-groups
  |=  [=ship =cone]
  ^-  (set path)
  %-  ~(gas in *(set path))
  %+  murn  ~(tap of (~(dip of cone) /grp/who))
  |=  [=path =grub]
  ^-  (unit ^path)
  =/  data=(each vase tang)  (grab-data-soft grub)
  ?:  ?=(%| -.data)  ~
  =/  res  (mule |.(!<((set @p) p.data)))
  ?:  ?=(%| -.res)  ~
  ?.  (~(has in p.res) ship)  ~
  [~ path]
::
++  merge-perms
  =|  =perm
  |=  perms=(list ^perm)
  ^+  perm
  ?~  perms
    perm
  %=  $
    perms      t.perms
    make.perm  (~(uni in make.perm) make.i.perms)
    poke.perm  (~(uni in poke.perm) poke.i.perms)
    peek.perm  (~(uni in peek.perm) peek.i.perms)
  ==
::
++  get-ship-perm
  |=  [=ship =cone]
  ^-  perm
  =/  groups=(set path)  (get-ship-groups ship cone)
  =/  groups-perm=(list perm)
    %+  murn  ~(tap of (~(dip of cone) /grp/how))
    |=  [=path =grub]
    ^-  (unit perm)
    ?.  (~(has in groups) path)  ~
    =/  data=(each vase tang)  (grab-data-soft grub)
    ?:  ?=(%| -.data)  ~
    =/  res  (mule |.(!<(perm p.data)))
    ?:(?=(%| -.res) ~ [~ p.res])
  =/  public-perm=perm
    ?~  grub=(~(get of cone) /grp/pub)  *perm
    =/  data=(each vase tang)  (grab-data-soft u.grub)
    ?:  ?=(%| -.data)  *perm
    =/  res  (mule |.(!<(perm p.data)))
    ?:(?=(%| -.res) *perm p.res)
  (merge-perms public-perm groups-perm)
::
++  grab-data-soft
  |=  =grub
  ^-  (each vase tang)
  ?-  -.kind.grub
    %base  &+data.kind.grub
    %stem  data.kind.grub
  ==
::
++  grab-data
  |=  =grub
  ^-  vase
  =/  res  (grab-data-soft grub)
  ?-  -.res
    %&  p.res
    %|  (mean p.res)
  ==
::
++  grab-data-as
  |*  [a=mold =grub]
  ^-  a
  !<(a (grab-data grub))
::
++  nead
  |*  a=(each)
  ?:  ?=(%& -.a)
    p.a
  (mean p.a)
::
++  send-raw-darts
  |=  darts=(list =dart)
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  [darts state %done ~]
::
++  send-raw-dart
  |=  =dart
  =/  m  (charm ,~)
  ^-  form:m
  (send-raw-darts dart ~)
::
++  send-wait
  |=  until=@da
  =/  m  (charm ,~)
  ^-  form:m
  %-  send-raw-dart
  [%sysc %pass /wait/(scot %da until) %arvo %b %wait until]
::
++  take-wake
  |=  until=(unit @da)
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %arvo [%wait @ ~] %behn %wake *]
    ?.  |(?=(~ until) =(`u.until (slaw %da i.t.wire.u.in)))
      [%skip ~]
    ?~  error.sign.u.in
      [%done ~]
    [%fail %timer-error u.error.sign.u.in]
  ==
::
++  wait
  |=  until=@da
  =/  m  (charm ,~)
  ^-  form:m
  ;<  ~  bind:m  (send-wait until)
  (take-wake `until)
::
++  sleep
  |=  for=@dr
  =/  m  (charm ,~)
  ^-  form:m
  ;<  now=@da  bind:m  get-time
  (wait (add now for))
::
++  take-poke-sign
  |=  =wire
  =/  m  (charm ,pail)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %base * %poke *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    ?:  ?=(%& -.p.sign.u.in)
      [%done p.p.sign.u.in]
    [%fail %poke-fail tang.p.p.sign.u.in]
  ==
::
++  poke
  |=  [=path =pail]
  =/  m  (charm ,^pail)
  =/  =dart  [%grub /poke path %poke pail]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-poke-sign /poke)
::
++  throw
  |=  [=path =pail]
  =/  m  (charm ,^pail)
  (send-raw-dart %grub / path %poke pail)
::
++  take-bump-sign
  |=  =wire
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %base * %bump *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    ?~  p.sign.u.in
      [%done ~]
    [%fail %poke-fail u.p.sign.u.in]
  ==
::
++  bump
  |=  [=path =pail]
  =/  m  (charm ,~)
  =/  =dart  [%grub /bump path %bump pail]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-bump-sign /poke)
::
++  take-bump-sign-soft
  |=  =wire
  =/  m  (charm ,(unit tang))
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %base * %bump *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    ?~  p.sign.u.in
      [%done ~]
    [%done ~ u.p.sign.u.in]
  ==
::
++  bump-soft
  |=  [=path =pail]
  =/  m  (charm ,(unit tang))
  =/  =dart  [%grub /bump path %bump pail]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-bump-sign-soft /poke)
::
++  take-peek
  |=  =wire
  =/  m  (charm ,[cone sand])
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %peek *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    [%done [cone sand]:u.in]
  ==
::
++  ls
  |=  =path
  =/  m  (charm ,(list @ta))
  ;<  =cone  bind:m  (peek path)
  (pure:m ~(tap in ~(key by dir.cone)))

:: list of the first non-empty descendents of a path
::
++  kids
  |=  =path
  =/  m  (charm ,(list ^path))
  ;<  =cone  bind:m  (peek path)
  =.  cone  (~(del of cone) /)
  =|  sub-path=^path
  =|  kids=(list ^path)
  %-  pure:m
  |-
  ^-  (list ^path)
  ?:  ?=(^ fil.cone)
    [sub-path kids]
  =/  dir  ~(tap by dir.cone)
  |-
  ^+   kids
  ?~  dir
    kids
  %=  $
    dir   t.dir
    kids  ^$(sub-path (weld sub-path /[p.i.dir]), cone q.i.dir)
  ==
:: tree information without the data
:: (avoids storing large amounts of data in the monad)
::
++  tree
  |=  =path
  =/  m  (charm ,(axal ~))
  ;<  =cone  bind:m  (peek path)
  %-  pure:m
  %-  ~(gas of *(axal ~))
  (turn ~(tap of cone) |=([p=^path *] [p ~]))
::
++  get-perm
  |=  =path
  =/  m  (charm ,(unit perm))
  =/  =dart  [%grub /get-perm path %peek ~]
  ;<  ~  bind:m  (send-raw-dart dart)
  ;<  [* =sand]  bind:m  (take-peek /get-perm)
  (pure:m (~(get of sand) /))
::
++  peek
  |=  =path
  =/  m  (charm ,cone)
  =/  =dart  [%grub /peek path %peek ~]
  ;<  ~  bind:m  (send-raw-dart dart)
  ;<  [=cone *]  bind:m  (take-peek /peek)
  (pure:m cone)
::
++  peek-root
  |=  =path
  =/  m  (charm ,grub)
  ;<  =cone  bind:m  (peek path)
  ?~  grub=(~(get of cone) /)
    (charm-fail %no-root-grub leaf+(spud path) ~)
  (pure:m u.grub)
::
++  peek-root-soft
  |=  =path
  =/  m  (charm ,(unit grub))
  ;<  =cone  bind:m  (peek path)
  (pure:m (~(get of cone) /))
::
++  peek-root-as
  |*  [a=mold =path]
  =/  m  (charm ,a)
  ;<  =grub  bind:m  (peek-root path)
  (pure:m !<(a (grab-data grub)))
::
++  peek-root-as-soft
  |*  [a=mold =path]
  =/  m  (charm ,(unit a))
  ;<  grub=(unit grub)  bind:m  (peek-root-soft path)
  ?~  grub
    (pure:m ~)
  (pure:m `!<(a (grab-data u.grub)))
:: peek, but with relative path
::
++  grab
  |=  =path
  =/  m  (charm ,cone)
  ;<  here=^path  bind:m  get-here
  (peek (weld here path))
::
++  grab-root
  |=  =path
  =/  m  (charm ,grub)
  ;<  here=^path  bind:m  get-here
  (peek-root (weld here path))
::
++  grab-root-as
  |*  [a=mold =path]
  =/  m  (charm ,a)
  ;<  here=^path  bind:m  get-here
  (peek-root-as (weld here path))
::
++  take-scry
  |*  [=mold =wire]
  =/  m  (charm ,mold)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %scry *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    [%done !<(mold vase.u.in)]
  ==
::
++  scry
  |*  [=mold =path]
  =/  m  (charm ,mold)
  =/  =dart  [%scry /scry mold path]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-scry mold /scry)
::
++  eyre-connect
  |=  [url=(list @t) dest=path]
  =/  m  (charm ,~)
  ;<  our=@p     bind:m  get-our
  (gall-poke [our %grubbery] connect+!>([url dest]))
::
++  eyre-disconnect
  |=  url=(list @t)
  =/  m  (charm ,~)
  ;<  our=@p     bind:m  get-our
  (gall-poke [our %grubbery] disconnect+!>(url))
::
++  oust-grub
  |=  =path
  =/  m  (charm ,~)
  =/  =dart  [%grub /oust-grub path %oust ~]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-gone /oust-grub)
::
++  take-gone
  |=  =wire
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %gone *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    ?~  err.u.in
      [%done ~]
    [%fail %oust-fail u.err.u.in]
  ==
::
++  edit-perm
  |=  [=path perm=(unit perm)]
  =/  m  (charm ,~)
  =/  =dart  [%grub /edit-perm path %sand perm]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-sand /edit-perm)
::
++  take-sand
  |=  =wire
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %sand *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    ?~  err.u.in
      [%done ~]
    [%fail %sand-fail u.err.u.in]
  ==
::
++  make-stem
  |=  [=path =stud stem=path sour=(set path)]
  =/  m  (charm ,~)
  =/  =dart  [%grub /make-stem path %make %stem stud stem sour]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-made /make-stem)
::
++  overwrite-stem
  |=  [=path =stud base=path sour=(set path)]
  =/  m  (charm ,~)
  ;<  ~  bind:m  (oust-grub path)
  (make-stem path stud base sour)
::
++  make-base
  |=  [=path =stud base=path data=(unit vase)]
  =/  m  (charm ,~)
  =/  =dart  [%grub /make-base path %make %base stud base data]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-made /make-base)
::
++  overwrite-base
  |=  [=path =stud base=path data=(unit vase)]
  =/  m  (charm ,~)
  ;<  ~  bind:m  (oust-grub path)
  (make-base path stud base data)
::
++  make-and-poke
  |=  [=path =stud base=path data=(unit vase) poke=pail]
  =/  m  (charm ,pail)
  ;<  ~  bind:m  (make-base path stud base data)
  (^poke path poke)
::
++  overwrite-and-poke
  |=  [=path =stud base=path data=(unit vase) poke=pail]
  =/  m  (charm ,pail)
  ;<  ~  bind:m  (overwrite-base path stud base data)
  (^poke path poke)
::
++  make-lib
  |=  [=path code=@t]
  =/  m  (charm ,~)
  ;<  *  bind:m 
    %:  make-and-poke
      [%lib path]
      /lib  /lib  ~
      [/sig !>(code)]
    ==
  (pure:m ~)
::
++  overwrite-lib
  |=  [=path code=@t]
  =/  m  (charm ,~)
  ;<  ~  bind:m  (edit-perm [%lib path] ~)
  ;<  ~  bind:m  (oust-grub [%lib path])
  (make-lib path code)
::
++  make-stud-lib 
  |=  [=path code=@t]
  =/  m  (charm ,~)
  (make-lib [%stud path] code)
::
++  overwrite-stud-lib
  |=  [=path code=@t]
  =/  m  (charm ,~)
  (overwrite-lib [%stud path] code)
::
++  make-base-lib 
  |=  [=path code=@t]
  =/  m  (charm ,~)
  (make-lib [%base path] code)
::
++  overwrite-base-lib
  |=  [=path code=@t]
  =/  m  (charm ,~)
  (overwrite-lib [%base path] code)
::
++  make-stem-lib 
  |=  [=path code=@t]
  =/  m  (charm ,~)
  (make-lib [%stem path] code)
::
++  overwrite-stem-lib
  |=  [=path code=@t]
  =/  m  (charm ,~)
  (overwrite-lib [%stem path] code)
::
++  take-made
  |=  =wire
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %made *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    ?~  err.u.in
      [%done ~]
    [%fail %make-fail u.err.u.in]
  ==
::
++  ignore
  |=  input
  [~ state %fail %ignore ~]
::
++  get-bowl
  =/  m  (charm ,bowl)
  ^-  form:m
  |=  input
  [~ state %done bowl]
::
++  get-time
  =/  m  (charm ,@da)
  ^-  form:m
  |=  input
  [~ state %done now.bowl]
::
++  get-our
  =/  m  (charm ,ship)
  ^-  form:m
  |=  input
  [~ state %done our.bowl]
::
++  get-entropy
  =/  m  (charm ,@uvJ)
  ^-  form:m
  |=  input
  [~ state %done eny.bowl]
::
++  get-from
  =/  m  (charm ,path)
  ^-  form:m
  |=  input
  [~ state %done from.bowl]
::
++  get-here
  =/  m  (charm ,path)
  ^-  form:m
  |=  input
  [~ state %done here.bowl]
::
++  get-state
  =/  m  (charm ,vase)
  ^-  form:m
  |=  input
  [~ state %done state]
::
++  get-state-as
  |*  a=mold
  =/  m  (charm ,a)
  ^-  form:m
  |=  input
  [~ state %done !<(a state)]
::
++  gut-state-as
  |*  a=mold
  |=  gut=$-(tang a)
  =/  m  (charm ,a)
  ^-  form:m
  |=  input
  =/  res  (mule |.(!<(a state)))
  ?-  -.res
    %&  [~ state %done p.res]
    %|  [~ state %done (gut p.res)]
  ==
::
:: ++  get-state-as
::   |*  a=mold
::   =/  m  (charm ,a)
::   ^-  form:m
::   |=  input
::   [~ state %done !<(a state)]
::
++  charm-fail
  |=  err=(pair term tang)
  |=  input
  [~ state %fail err]
::
++  transform
  |=  transform=$-(vase vase)
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  ^-  output:m
  [~ (transform state) %done ~]
::
++  replace
  |=  new=vase
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  ^-  output:m
  [~ new %done ~]
::
++  commit
  |=  new=vase
  =/  m  (charm ,~)
  ^-  form:m
  ;<  ~  bind:m  (replace new)
  :: when this card leaves %grubbery
  :: the state will have officially changed and
  :: all local dependency changes will have propagated
  (sleep ~s0)
:: do nothing and give a sig
::
++  done
  =/  m  (charm ,pail)
  (pure:m /sig !>(~))
:: replace with value and give a sig
::
++  pour
  |=  new=vase
  =/  m  (charm ,pail)
  ;<  ~  bind:m  (replace new)
  done
::
++  gall-poke
  |=  [=dock =cage]
  =/  m  (charm ,~)
  ^-  form:m
  =/  =dart  [%sysc %pass /poke %agent dock %poke cage]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-poke-ack /poke)
::
++  gall-poke-our
  |=  [=dude:gall =cage]
  =/  m  (charm ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  (gall-poke [our dude] cage)
::
++  handle-http-response
  |=  [eyre-id=@ta pay=simple-payload:http]
  =/  m  (charm ,~)
  ^-  form:m
  %+  gall-poke-our
    %grubbery
  handle-http-response+!>([eyre-id pay])
::
++  final-http-response
  |=  [eyre-id=@ta pay=simple-payload:http]
  =/  m  (charm ,pail)
  ^-  form:m
  ;<  ~  bind:m  (handle-http-response eyre-id pay)
  done
::
++  take-poke-ack
  |=  =wire
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %agent * %poke-ack *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    ?~  p.sign.u.in
      [%done ~]
    [%fail %poke-fail u.p.sign.u.in]
  ==
::
++  watch
  |=  [=wire =dock =path]
  =/  m  (charm ,~)
  ^-  form:m
  ;<  ~  bind:m
    (send-raw-dart %sysc %pass watch+wire %agent dock %watch path)
  (take-watch-ack wire)
::
++  leave
  |=  [=wire =dock]
  =/  m  (charm ,~)
  ^-  form:m
  (send-raw-dart %sysc %pass watch+wire %agent dock %leave ~)
::
++  take-watch-ack
  |=  =wire
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %agent * %watch-ack *]
    ?.  =(watch+wire wire.u.in)
      [%skip ~]
    ?~  p.sign.u.in
      [%done ~]
    [%fail %watch-ack-fail u.p.sign.u.in]
  ==
::
++  take-fact
  |=  =wire
  =/  m  (charm ,cage)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %agent * %fact *]
    ?.  =(watch+wire wire.u.in)
      [%skip ~]
    [%done cage.sign.u.in]
  ==
::
++  take-kick
  |=  =wire
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %agent * %kick *]
    ?.  =(watch+wire wire.u.in)
      [%skip ~]
    [%done ~]
  ==
::
++  render-tang-to-wall
  |=  [wid=@u tan=tang]
  ^-  wall
  (zing (turn tan |=(a=tank (wash 0^wid a))))
::
++  render-tang-to-marl
  |=  [wid=@u tan=tang]
  ^-  marl
  =/  raw=(list tape)  (zing (turn tan |=(a=tank (wash 0^wid a))))
  ::
  |-  ^-  marl
  ?~  raw  ~
  [;/(i.raw) ;br; $(raw t.raw)]
::
++  internal-server-error
  |=  [authorized=? msg=tape t=tang]
  ^-  simple-payload:http
  =;  =manx
    :_  `(manx-to-octs:server manx)
    [500 ['content-type' 'text/html']~]
  ;html
    ;head
      ;title:"500 Internal Server Error"
    ==
    ;body
      ;h1:"Internal Server Error"
      ;p: {msg}
      ;*  ?:  authorized
            ;=
              ;code:"*{(render-tang-to-marl 80 t)}"
            ==
          ~
    ==
  ==
::
++  method-not-allowed
  |=  method=@t
  ^-  simple-payload:http
  =;  =manx
    :_  `(manx-to-octs:server manx)
    [405 ['content-type' 'text/html']~]
  ;html
    ;head
      ;title:"405 Method Not Allowed"
    ==
    ;body
      ;h1:"Method Not Allowed: {(trip method)}"
    ==
  ==
::
++  two-oh-four
  ^-  simple-payload:http
  [[204 ['content-type' 'application/json']~] ~]
::
++  give-manx-response
  |=  =manx
  =/  m  (charm ,pail)
  ^-  form:m
  %+  pure:m  /simple-payload  !>
  (manx-response:gen:server manx)
--
