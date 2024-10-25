/-  *signals
=,  root
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
++  make-stem
  |=  [=path stud=path sour=(set path)]
  =/  m  (charm ,~)
  =/  =dart  [%node /make-stem path %make %stem stud sour]
  ;<  ~  bind:m  (send-raw-dart dart)
  (take-made /make-stem)
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
--
