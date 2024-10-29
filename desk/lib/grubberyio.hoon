/-  *grubbery
/+  server
=,  base
|%
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
  ~&  %taking-poke-sign
  ~&  input++<
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
  ~&  >>  %sent-card
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
  =/  m  (charm ,cone)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %peek *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    [%done cone.u.in]
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
++  peek
  |=  =path
  =/  m  (charm ,cone)
  =/  =dart  [%grub /peek path %peek ~]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-peek /peek)
::
++  peek-root
  |=  =path
  =/  m  (charm ,grub)
  ;<  =cone  bind:m  (peek path)
  ?~  grub=(~(get of cone) /)
    (charm-fail %no-root-grub leaf+(spud path) ~)
  (pure:m u.grub)
::
++  peek-root-as
  |*  [a=mold =path]
  =/  m  (charm ,a)
  ;<  =grub  bind:m  (peek-root path)
  (pure:m !<(a data.grub))
::
++  peek-root-soft
  |=  =path
  =/  m  (charm ,(unit grub))
  ;<  =cone  bind:m  (peek path)
  (pure:m (~(get of cone) /))
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
  |=  url=(list @t)
  =/  m  (charm ,~)
  =/  =dart  [%sysc %pass /connect %arvo %e %connect `url %grubbery]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-connect /connect)
::
++  eyre-disconnect
  |=  url=(list @t)
  =/  m  (charm ,~)
  (send-raw-dart %sysc %pass /connect %arvo %e %disconnect `url)
::
++  take-connect
  |=  =wire
  =/  m  (charm ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %arvo * %eyre %bound *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    ?:  accepted.sign.u.in
      ~&  >  "{(spud path.binding.sign.u.in)} bound successfully!"
      [%done ~]
    ~&  >>>  "Binding {(spud path.binding.sign.u.in)} failed!"
    [%done ~]
  ==
::
++  make-stem
  |=  [=path =stud stem=path sour=(set path)]
  =/  m  (charm ,~)
  =/  =dart  [%grub /make-stem path %make %stem stud stem sour]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-made /make-stem)
::
++  make-base
  |=  [=path =stud base=path data=(unit vase)]
  =/  m  (charm ,~)
  =/  =dart  [%grub /make-base path %make %base stud base data]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-made /make-base)
::
++  make-and-poke
  |=  [=path =stud base=path data=(unit vase) poke=pail]
  =/  m  (charm ,pail)
  ;<  ~  bind:m  (make-base path stud base data)
  (^poke path poke)
::
++  make-lib
  |=  [=path code=@t]
  =/  m  (charm ,~)
  ;<  *  bind:m 
    %:  make-and-poke
      (weld /lib path)
      /lib  /lib  ~
      [/sig !>(code)]
    ==
  (pure:m ~)
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
  ~&  %taking-poke-ack
  :+  ~  state
  ?+  in  [%skip ~]
      ~  [%wait ~]
      [~ %agent * %poke-ack *]
    ?.  =(wire wire.u.in)
      [%skip ~]
    ~&  %got-a-poke-ack-for-real
    ~&  p.sign.u.in
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
--
