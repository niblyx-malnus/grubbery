|%
+$  stud  path
+$  pail  (pair stud vase)
+$  card  card:agent:gall
+$  make
  $%  [%base =stud base=path data=(unit vase)]
      [%stem =stud stem=path sour=(set path)]
  ==
:: add a %cull action as a recursive %oust
::
+$  deed  ?(%make %oust %poke %bump %kill %peek)
::
+$  perm
  $:  make=(set path) :: %make or %oust (%sand ?)
      poke=(set path) :: %poke, %bump or %kill
      peek=(set path)
  ==
::
+$  sand  (axal perm)
::
:: effects that a base grub can emit
::
+$  dart
  $%  [%grub =wire =path =load]
      [%sysc =card:agent:gall]
      [%scry =wire =mold =path]
  ==
::
+$  bolt  (pair path dart)
:: dart payload
::
+$  load
  $%  [%make =make]
      [%oust ~]
      [%poke =pail]
      [%bump =pail]
      [%kill ~]
      [%peek ~]
  ==
::
+$  kind
  $%  [%base data=vase base=path proc=(unit proc:base)]
      [%stem data=(each vase tang) stem=path tidy=? sour=(map path @da)]
  ==
::
+$  grub  [=stud =kind]
+$  cone  (axal grub)
+$  give  [from=path =wire]
+$  poke  [=give =pail]
+$  tack
  $:  last=[step=@da poke=@da]
      sinx=(set path)
      give=(unit give)
      line=(qeu poke)
  ==
+$  trac  (axal tack)
:: NOTE: the distinction between cone and trac exists because
::       it is not yet clear what information should be available
::       on peek. Sinx being in tack also makes dependencies
::       more ergonomic (for now) in the case where a source
::       is deleted and then recreated or replaced
::
+$  bindings  (map (list @t) path) :: eyre bindings
:: time ordered latest changes
::
+$  history   ((mop @da path) gth)
++  hon       ((on @da path) gth)
::
++  stem
  =<  stem
  |%
  +$  stem  $-(bowl (quip dart vase))
  +$  bowl
    $:  now=@da                          :: time
        our=@p                           :: host
        eny=@uvJ                         :: entropy
        here=path                        :: our address
        deps=(map path (each vase tang)) :: dependencies
    ==
  --
::
++  base
  =<  base
  |%
  +$  bowl
    $:  now=@da       :: time
        our=@p        :: host
        eny=@uvJ      :: entropy
        wex=boat:gall :: outgoing gall subs
        sup=bitt:gall :: incoming gall subs
        from=path     :: provenance
        here=path     :: our address
    ==
  ::
  +$  sign
    $%  [%poke p=(each pail goof)]
        [%bump p=(unit tang)]
    ==
  ::
  +$  intake
    $%  [%bump =pail]
        [%peek =wire =path =cone] :: local read
        [%made =wire err=(unit tang)] :: response to make
        [%gone =wire err=(unit tang)] :: response to oust
        [%dead =wire err=(unit tang)] :: response to kill
        [%base =wire =sign] :: response from poke or bump
        :: messages from gall and arvo
        ::
        [%scry =wire =path =vase]
        [%arvo =wire sign=sign-arvo]
        [%agent =wire =sign:agent:gall]
        [%watch =path]
        [%leave =path]
    ==
  ::
  +$  input  [=bowl state=vase in=(unit intake)]
  ::
  ++  output-raw
    |*  value=mold
    $~  [~ !>(~) %done *value]
    $:  darts=(list dart)
        state=vase
        $=  next
        $%  [%wait ~]
            [%skip ~]
            [%cont self=(form-raw value)]
            [%fail err=(pair term tang)]
            [%done =value]
        ==
    ==
  ::
  ++  form-raw
    |*  value=mold
    $-(input (output-raw value))
  ::
  +$  proc  _*form:(charm ,pail)
  +$  base  $-([bowl pail] proc)
  +$  give  (each @ta [from=path =wire])
  +$  poke  [=give =pail]
  :: 
  ++  charm
    |*  value=mold
    |%
    ++  output  (output-raw value)
    ++  form    (form-raw value)
    :: give value; leave state unchanged
    ::
    ++  pure
      |=  =value
      ^-  form
      |=  input
      ^-  output
      [~ state %done value]
    ::
    ++  bind
      |*  b=mold
      |=  [m-b=(form-raw b) fun=$-(b form)]
      ^-  form
      |=  =input
      =/  b-res=(output-raw b)  (m-b input)
      ^-  output
      :-  darts.b-res
      :-  state.b-res
      ?-    -.next.b-res
        %wait  [%wait ~]
        %skip  [%skip ~]
        %cont  [%cont ..$(m-b self.next.b-res)]
        %fail  [%fail err.next.b-res]
        %done  [%cont (fun value.next.b-res)]
      ==
    ::
    ++  eval
      |%
      +$  result
        $%  [%next ~]
            [%fail err=(pair term tang)]
            [%done =value]
        ==
      ::
      ++  take
        =|  darts=(list dart)
        |=  [=form =input]
        ^-  [[(list dart) vase result] _form]
        =/  =output  (form input)
        =.  darts  (weld darts darts.output)
        ?:  ?=(%cont -.next.output)
          %=  $
            form   self.next.output
            input  [bowl.input state.output ~]
          ==
        :_  form
        :-  darts
        :-  state.output
        ?-  -.next.output
            %wait  [%next ~]
            %skip  [%next ~]
            %fail  [%fail err.next.output]
            %done  [%done value.next.output]
        ==
      --
    --
  --
--
