/-  s=signals
/+  *signalsio
|%
++  slip
  |=  [vax=vase gen=hoon]
  ^-  vase
  =+  gun=(~(mint ut p.vax) %noun gen)
  [p.gun (need (mack q.vax q.gun))]
::
++  bin
  |%
  ++  stem
    ^-  stem:s
    |=  =bowl:stem:s
    ^-  (quip card:s vase)
    ?>  ?=([%bin *] here.bowl)
    =/  file=vase  (~(got by deps.bowl) (welp /fil t.here.bowl))
    =+  !<([@t deps=(list (pair term path)) =hoon] file)
    ?>  .=  ~(key by deps.bowl)
        (~(put in (sy (turn deps tail))) (welp /fil t.here.bowl))
    =;  vax=(list vase)
      =.  vax  (snoc vax (slop !>(s=s) !>(..zuse)))
      [~ (slip (reel vax slop) hoon)]
    %+  turn  deps
    |=  [fac=term dep=path]
    =/  =vase  (~(got by deps.bowl) dep)
    vase(p [%face fac p.vase])
  ::
  ++  stem-2
    ^-  stem-2:s
    |=  =bowl:stem:s
    ^-  (quip card-2:s vase)
    ?>  ?=([%bin *] here.bowl)
    =/  file=vase  (~(got by deps.bowl) (welp /fil t.here.bowl))
    =+  !<([@t deps=(list (pair term path)) =hoon] file)
    ?>  .=  ~(key by deps.bowl)
        (~(put in (sy (turn deps tail))) (welp /fil t.here.bowl))
    =;  vax=(list vase)
      =.  vax  (snoc vax (slop !>(s=s) !>(..zuse)))
      [~ (slip (reel vax slop) hoon)]
    %+  turn  deps
    |=  [fac=term dep=path]
    =/  =vase  (~(got by deps.bowl) dep)
    vase(p [%face fac p.vase])
  --
::
++  fil
  |%
  ++  root-2
    ^-  root:proc:s
    |=  [=bowl:proc:s =stud:s =vase]
    =/  m  (charm:proc:s ,pail:s)
    ^-  form:m
    ~&  >>  %test
    ?+    stud  !!
        [%step ~]
      ~&  >>  %step
      =/  =@t  !<(@t vase)
      ;<  ~    bind:m  (replace !>([t (build t)]))
      give-sig
      ::
        [%init ~]
      ~&  >>  %initing
      ;<  =@t  bind:m  (get-state-as @t)
      ~&  >>  %initing-a
      =/  [pax=(list (pair term path)) =hoon]  (build t)
      ~&  >>  %initing-b
      ;<  ~    bind:m  (replace !>([t pax hoon]))
      ~&  >>  %initing-c
      ?>  ?=([%fil *] here.bowl)
      ~&  >>  %initing-d
      =/  dest=path  (welp /bin t.here.bowl)
      ~&  >>  %initing-e
      =/  sour=(set path)  (sy (turn pax tail))
      ~&  >>  %initing-f
      ;<  ~  bind:m
        (gall-poke [our.bowl %signals] make-stem-2+!>([dest /bin sour]))
      ~&  >>  %initing-g
      give-sig
    ==
  ::
  ++  root
    ^-  root:s
    |=  [=bowl:root:s data=vase =stud:s =vase]
    ?>  ?=([%fil *] here.bowl)
    :-  ~
    ?+    stud  !!
        [%init ~]
      =+  !<(=@t data)
      !>([t (build t)])
      ::
        [%put ~]
      =+  !<(=@t vase)
      !>([t (build t)])
    ==
  :: TODO: allow optionally setting a path prefix
  ::
  ++  import-line
    ;~  plug
      (cook term ;~(pfix ;~(plug (jest '/-') gap) sym))
      (cook |=(=path (welp /bin path)) ;~(pfix ;~(plug gap fas) (more fas sym)))
    ==
   ::
   ++  build
     |=  text=@t
     ^-  [(list (pair term path)) hoon]
     (rash text ;~(pfix (star gap) ;~(plug (more gap import-line) vest)))
  --
::
++  counter
  '''
  |=  [=bowl:proc:s:a =stud:s:a =vase]
  =/  m  (charm:proc:s:a ,pail:s:a)
  ^-  form:m
  ?+    stud  !!
      [%init ~]
    give-sig
    ::
      [%inc ~]
    ;<  a=@ud  bind:m  (get-state-as @ud)
    ;<  ~      bind:m  (replace !>(+(a)))
    give-sig
    ::
      [%two ~]
    ;<  a=@ud  bind:m  (get-state-as @ud)
    ;<  ~      bind:m  (replace !>(+(a)))
    give-sig
  ==
  '''
::
++  is-even
  '''
  |=  =bowl:stem-2:s
  :-  ~
  =+  !<(=@ud (~(got by deps.bowl) /counter))
  !>((mod ud 2))
  '''
::
++  parity
  '''
  |=  =bowl:stem-2:s
  :-  ~
  !>
  ?:  =(0 !<(@ud (~(got by deps.bowl) /is-even)))
    'true'
  'false'
  '''
::
++  add-two
  '''
  |%
  ++  add-two  |=(a=@ud (add a 2))
  --
  '''
--
