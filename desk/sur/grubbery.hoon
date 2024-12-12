|%
+$  stud  path
+$  pail  (pair stud vase)
+$  card  card:agent:gall
+$  make
  $%  [%base base=path data=(unit vase)]
      [%stem stem=path sour=(set path)]
  ==
::
+$  deed  ?(%make %oust %cull %sand %poke %bump %kill %peek)
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
      :: intermediate poke response;
      :: like a bump, but automatically goes to poker
      :: and is not sandboxed
      :: can also think of a poke like a subscription
      :: which can return perks and a final pail
      :: 
      [%perk =wire =pail] :: TODO: replace poke response with perk?
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
      [%cull ~]
      [%sand perm=(unit perm)]
      [%poke =pail]
      [%bump =pail]
      [%kill ~]
      [%peek ~]
  ==
:: TODO: don't queue processes, they should all be going
::       simultaneously
:: TODO: each base should be able to publish data to
::       subscription paths
::
+$  grub
  $%  [%base data=vase base=path proc=(unit proc:base)]
      [%stem data=(each vase tang) stem=path tidy=? sour=(map path @da)]
  ==
::
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
    $%  [%poke err=(unit tang)]
        [%perk err=(unit tang)]
        [%bump err=(unit tang)]
    ==
  ::
  +$  intake
    $%  [%bump =pail]
        [%perk =wire =pail]
        [%peek =wire =path =cone =sand] :: local read
        [%made =wire err=(unit tang)] :: response to make
        [%gone =wire err=(unit tang)] :: response to oust
        [%cull =wire err=(unit tang)] :: response to cull
        [%dead =wire err=(unit tang)] :: response to kill
        [%sand =wire err=(unit tang)] :: response to sand
        [%base =wire =sign] :: response from poke or bump
        [%veto =dart] :: notify that a dart was sandboxed
        :: messages from gall and arvo
        ::
        [%scry =wire =path =vase]
        [%arvo =wire sign=sign-arvo]
        [%agent =wire =sign:agent:gall]
        [%watch =path] :: disallow process subs
        [%leave =path] :: disallow process subs
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
            [%cont self=(form-raw value)]
            [%fail err=tang]
            [%done =value]
        ==
    ==
  ::
  ++  form-raw
    |*  value=mold
    $-(input (output-raw value))
  ::
  +$  proc  _*form:(charm ,~)
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
        %cont  [%cont ..$(m-b self.next.b-res)]
        %fail  [%fail err.next.b-res]
        %done  [%cont (fun value.next.b-res)]
      ==
    ::
    ++  eval
      |%
      +$  result
        $%  [%next ~]
            [%fail err=tang]
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
            %fail  [%fail err.next.output]
            %done  [%done value.next.output]
        ==
      --
    --
  --
--
