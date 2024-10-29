/-  g=grubbery
/+  grubberyio, server, html-utils
|%
++  mx  mx:html-utils
++  kv  kv:html-utils
++  slip
  |=  [vax=vase gen=hoon]
  ^-  vase
  =+  gun=(~(mint ut p.vax) %noun gen)
  [p.gun (need (mack q.vax q.gun))]
::
++  bin
  |%
  ++  stem
    ^-  stem:g
    |=  =bowl:stem:g
    ^-  (quip dart:g vase)
    ?>  ?=([%bin *] here.bowl)
    =/  file=vase  (~(got by deps.bowl) (welp /lib t.here.bowl))
    =+  !<([@t res=(each [deps=(list (pair term path)) =hoon] tang)] file)
    ?:  ?=(%| -.res)
      ~|("hoon parsing failure" !!)
    ?>  .=  ~(key by deps.bowl)
        (~(put in (sy (turn deps.p.res tail))) (welp /lib t.here.bowl))
    =;  vax=(list vase)
      =.  vax  (snoc vax !>(..bin))
      [~ (slip (reel vax slop) hoon.p.res)]
    %+  turn  deps.p.res
    |=  [fac=term dep=path]
    =/  =vase  (~(got by deps.bowl) dep)
    vase(p [%face fac p.vase])
  --
::
++  lib
  |%
  ++  base
    =,  grubberyio
    ^-  base:g
    |=  [=bowl:base:g =stud:g =vase]
    =/  m  (charm:base:g ,pail:g)
    ^-  form:m
    ?+    stud  !!
        [%sig ~]
      =+  !<(=@t vase)
      =/  res=(each [pax=(list (pair term path)) =hoon] tang)
        (mule |.((build t)))
      ;<  ~  bind:m  (replace !>([t res]))
      ?>  ?=([%lib *] here.bowl)
      =/  dest=path  (welp /bin t.here.bowl)
      =/  sour=(set path)
        ?:(?=(%| -.res) ~ (sy (turn pax.p.res tail)))
      =.  sour  (~(put in sour) here.bowl)
      ;<  ~  bind:m  (make-stem dest /bin /bin sour)
      done
    ==
  :: TODO: allow optional face and relative paths (i.e. /^/^/path)
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
++  boot
  =,  grubberyio
  ^-  base:g
  |=  [=bowl:base:g =stud:g =vase]
  =/  m  (charm:base:g ,pail:g)
  ^-  form:m
  ?+    stud  !!
      [%sig ~]
    ;<  ~  bind:m  (make-lib /stud/ud '@ud')
    ;<  ~  bind:m  (make-lib /stud/loob '?')
    ;<  ~  bind:m  (make-lib /stud/txt '@t')
    ;<  ~  bind:m  (make-lib /stud/dr 'dr')
    ;<  ~  bind:m  (make-lib /stud/manx 'manx')
    :: counter test
    ::
    ;<  ~  bind:m  (make-lib /add/two add-two)
    ;<  ~  bind:m  (make-lib /counter counter)
    ;<  ~  bind:m  (make-lib /is-even is-even)
    ;<  ~  bind:m  (make-lib /parity parity)
    ;<  ~  bind:m  (make-base /counter /ud /counter `!>(10))
    ;<  ~  bind:m  (make-stem /is-even /loob /is-even (sy ~[/counter]))
    ;<  ~  bind:m  (make-stem /parity /txt /parity (sy ~[/is-even]))
    :: gui setup
    ::
    ;<  ~  bind:m  (make-lib /base/gui 'base:gui')
    ;<  ~  bind:m  (make-lib /stud/gui/init ',~')
    ;<  *  bind:m  (make-and-poke /gui /sig /gui ~ /gui/init !>(~))
    ~&  >  "Grubbery booted!"
    done
  ==
::
++  gui
  |%
  ++  make-id  |=(p=path (trip (rap 3 (join '_' p))))
  ::
  ++  counter
    |=  =bowl:stem:g
    ^-  (quip dart:g vase)
    :-  ~
    =+  !<(=@ud (~(got by deps.bowl) /counter))
    !>
    ;div
      =id     (make-id here.bowl)
      =class  "flex flex-row gap-2 justify-center"
      ;div(class "text-4xl font-bold mb-6")
        ; counter: {(scow %ud ud)}
      ==
      ;form
        =method  "POST"
        =action  "/grub?action=inc"
        ;button
          =type   "submit"
          =class  "px-3 py-2 bg-blue-500 text-white font-semibold text-lg rounded-lg hover:bg-blue-700 transition duration-200"
          ; +
        ==
      ==
    ==
  ::
  ++  is-even
    |=  =bowl:stem:g
    ^-  (quip dart:g vase)
    :-  ~
    =+  !<(e=? (~(got by deps.bowl) /is-even))
    !>
    ^-  manx
    ;div
      =id     (make-id here.bowl)
      =class  "text-4xl font-bold mb-6"
      ; is-even: {?:(e "%.y" "%.n")}
    ==
  ::
  ++  parity
    |=  =bowl:stem:g
    ^-  (quip dart:g vase)
    :-  ~
    =+  !<(=@t (~(got by deps.bowl) /parity))
    !>
    ;div
      =id     (make-id here.bowl)
      =class  "text-4xl font-bold mb-6"
      ; parity: {(trip t)}
    ==
  ::
  ++  refresher
    =,  grubberyio
    ^-  base:g
    =<
    |=  [=bowl:base:g =stud:g =vase]
    =/  m  (charm:base:g ,pail:g)
    ^-  form:m
    ?+    stud  !!
        [%interval ~]
      (pour !>(!<(@dr vase)))
      ::
        [%handle-http-request ~]
      =+  !<([request-line:server req=inbound-request:eyre] vase)
      ?+    method.request.req
        %+  pure:m  /simple-payload
        !>((method-not-allowed method.request.req))
        ::
          %'GET'
        =/  since=@da  (slav %da (need (get-key:kv 'since' args)))
        ;<  now=@da  bind:m  get-time
        ;<  interval=@dr  bind:m  (peek-root-as @dr here.bowl)
        =/  until=@da  (add now interval)
        ;<  ~  bind:m  (send-wait until)
        ;<  ~  bind:m  take-wake-or-fast-refresh
        (send-refresh-list since)
      ==
    ==
    ::
    |%
    ++  take-wake-or-fast-refresh
      =/  m  (charm ,~)
      ^-  form:m
      |=  input
      :+  ~  state
      ?+    in  [%skip ~]
        ~  [%wait ~]
        ::
          [~ %arvo [%wait @ ~] %behn %wake *]
        ?~  error.sign.u.in
          [%done ~]
        [%fail %timer-error u.error.sign.u.in]
        ::
          [~ %bump *]
        ?.  =(p.pail.u.in /fast-refresh)
          [%skip ~]
        [%done ~]
      ==
    ::
    ++  send-refresh-list
      |=  since=@da
      =/  m  (charm ,pail)
      ^-  form:m
      ;<  history=(list (pair @da path))  bind:m
        %+  scry
          (list (pair @da path))
        /gx/grubbery/history/(scot %da since)/noun
      ~&  >  history+history
      ;<  now=@da  bind:m  get-time
      =/  new-since=@da  ?~(history now p.i.history)
      %+  pure:m  /simple-payload  !>
      %-  manx-response:gen:server 
      (refresher-component:gui new-since (turn history tail))
    --
  ::
  ++  refresher-component
    |=  [since=@da refresh=(list path)]
    ^-  manx
    =.  refresh
      %+  murn  refresh
      |=  =path
      ?.  ?=([%gui %dom *] path)
        ~
      `path
    :: if it's here in our dom folder, update it
    ;div#refresher
      =style  "display: none;"
      =hx-target         "this"
      =hx-get            "/grub/refresher?since={(scow %da since)}"
      =hx-trigger        "load"
      =hx-swap           "outerHTML"
      ;*  =|  id=@
          |-
          ?~  refresh
             ~
          :_  $(refresh t.refresh)
          ;div
            =id            "refresh-fetcher-{(scow %ud id)}"
            =style         "display: none;"
            =hx-target     "#{(make-id i.refresh)}"
            =hx-get        "{(spud i.refresh)}"
            =hx-indicator  "#{(make-id i.refresh)} .loader"
            =hx-swap       "outerHTML"
            =hx-trigger    "click";
    ==
  ::
  ++  session
    =,  grubberyio
    ^-  base:g
    |=  [=bowl:base:g =stud:g =vase]
    =/  m  (charm:base:g ,pail:g)
    ^-  form:m
    ?+    stud  !!
        [%handle-http-request ~]
      =+  !<([request-line:server req=inbound-request:eyre] vase)
      ?+    method.request.req
        %+  pure:m  /simple-payload
        !>((method-not-allowed method.request.req))
        ::
          %'GET'
        ~&  site+site
        ?+    site  !!
          ::
            [%grub %lib *]
          %+  pure:m  /simple-payload
          !>((manx-response:gen:server *manx))
        ==
      ==
    ==
  ::
  ++  base
    =,  grubberyio
    ^-  base:g
    =<
    |=  [=bowl:base:g =stud:g =vase]
    =/  m  (charm:base:g ,pail:g)
    ^-  form:m
    =/  refresher=path  (weld here.bowl /refresher)
    ?+    stud  !!
        [%gui %init ~]
      ;<  ~  bind:m  (eyre-connect /grub)
      ;<  *  bind:m  (make-lib /gui/refresher 'refresher:gui')
      ;<  ~  bind:m  (make-base refresher /dr /gui/refresher `!>(~s5))
      ;<  ~  bind:m  (make-lib /dom/counter 'counter:gui')
      ;<  ~  bind:m
        (make-stem (weld here.bowl /dom/counter) /manx /dom/counter (sy ~[/counter]))
      ;<  ~  bind:m  (make-lib /dom/is-even 'is-even:gui')
      ;<  ~  bind:m
        (make-stem (weld here.bowl /dom/is-even) /manx /dom/is-even (sy ~[/is-even])) 
      ;<  ~  bind:m  (make-lib /dom/parity 'parity:gui')
      ;<  ~  bind:m
        (make-stem (weld here.bowl /dom/parity) /manx /dom/is-even (sy ~[/parity]))
      done
      ::
        [%handle-http-request ~]
      =+  !<([request-line:server req=inbound-request:eyre] vase)
      ?+    method.request.req
        %+  pure:m  /simple-payload
        !>((method-not-allowed method.request.req))
        ::
          %'GET'
        ~&  site+site
        ?+    site
          ;<  =cone:g   bind:m  (peek /)
          %+  pure:m  /simple-payload
          !>((manx-response:gen:server (main-page cone)))
          ::
          ::   [%grub *]
          :: =/  src=@t
          ::   ?:  ?=([@ %eyre @ @ ~] from.bowl)
          ::     i.t.t.from.bowl
          ::   (scot %p our.bowl)
          :: ;<  contents=(list @ta)  bind:m
          ::   (ls (weld here.bowl /[src]))
          :: =/  sessions=(list @ud)
          ::   (sort (turn contents (cury slav %ud)) gth)
          :: ?~  sessions
            :: $-(path [sour=(set path) stem])
            :: create a new session
            :: !!
          :: check if
          :: =/  new=path  
          :: :: redirects to last session and creates one if it 
          :: :: doesn't exist
          :: (redirect:gen:server (spat ))
          :: !!
          ::
            [%grub %make %base ~]
          %+  pure:m  /simple-payload
          !>((manx-response:gen:server (wrap-manx make-base-interface)))
          ::
            [%grub %tree %lib *]
          ;<  g=(unit grub:g)  bind:m  (peek-root-soft t.site)
          ;<  ~  bind:m
            ?^(g (pure:(charm ,~) ~) (make-lib t.t.site ''))
          ;<  =manx  bind:m  (make-lib-page t.t.site)
          %+  pure:m  /simple-payload
          !>((manx-response:gen:server (wrap-manx manx)))
          ::
            [%grub %counter ~]
          ;<  now=@da        bind:m  get-time
          ;<  counter=manx   bind:m  (peek-root-as manx (weld here.bowl /dom/counter))
          ;<  is-even=manx   bind:m  (peek-root-as manx (weld here.bowl /dom/is-even))
          ;<  parity=manx    bind:m  (peek-root-as manx (weld here.bowl /dom/parity))
          %+  pure:m  /simple-payload  !>
          %-  manx-response:gen:server 
          (counter-page now counter is-even parity)
          ::
            [%grub %dom *]
          ;<  =manx   bind:m  (get-dom-manx t.t.site)
          %+  pure:m  /simple-payload  !>
          (manx-response:gen:server manx)
        ==
        ::
          %'POST'
        ?+    site
          =/  action=@t  (need (get-key:kv 'action' args))
          ;<  *  bind:m  (poke /counter /inc !>(~))
          ~&  >>  %poked-counter
          ;<  ~  bind:m
            (send-raw-dart %grub / refresher %bump /fast-refresh !>(~))
          (pure:m /simple-payload !>(two-oh-four))
          ::
            [%grub %make %base ~]
          =/  args=key-value-list:kv  (parse-body:kv body.request.req) 
          =/  =path       (rash (need (get-key:kv 'path' args)) stap)
          =/  =stud:g     (rash (need (get-key:kv 'stud' args)) stap)
          =/  base=^path  (rash (need (get-key:kv 'base' args)) stap)
          ;<  ~  bind:m  (make-base path stud base ~)
          (pure:m /simple-payload !>(two-oh-four))
          ::
            [%grub %make %lib ~]
          =/  args=key-value-list:kv  (parse-body:kv body.request.req) 
          =/  =path    (rash (need (get-key:kv 'path' args)) stap)
          =/  code=@t  (need (get-key:kv 'code' args))
          ;<  ~  bind:m  (make-lib path code)
          ;<  =manx  bind:m  (make-lib-page path)
          %+  pure:m  /simple-payload
          !>((manx-response:gen:server manx))
        ==
      ==
    ==
    ::
    |%
    ++  wrap-manx
      |=  =manx
      ^+  manx
      ;html(lang "en")
        ;head
          ;meta(charset "UTF-8");
          ;meta(name "viewport", content "width=device-width, initial-scale=1.0");
          ;title: Grubbery
          ;script(src "https://cdn.tailwindcss.com");
          ;script(src "https://unpkg.com/htmx.org@1.9.4");
        ==
        ;body
          ;+  manx
        ==
      ==
    ::
    ++  make-base-interface
      ^-  manx
      ;div(class "max-w-md mx-auto p-6 bg-white shadow-lg rounded-lg mt-10 border border-gray-200")
        ;h2(class "text-2xl font-bold text-gray-700 mb-6 text-center"): Make Base
        ;form
          =class  "space-y-4"
          =hx-post  "/grub/make/base"
          =hx-indicator  "#loading-indicator"
          =hx-swap  "none"
          ;div
            ;label
              =for  "path"
              =class  "block text-gray-700 font-semibold mb-1"
              Path
            ==
            ;input
              =type  "text"
              =id  "path"
              =name  "path"
              =required  ""
              =class  "w-full p-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
              ;
            ==
          ==
          ;div
            ;label
              =for  "stud"
              =class  "block text-gray-700 font-semibold mb-1"
              Stud
            ==
            ;input
              =type  "text"
              =id  "stud"
              =name  "stud"
              =required  ""
              =class  "w-full p-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
              ;
            ==
          ==
          ;div
            ;label
              =for  "base"
              =class  "block text-gray-700 font-semibold mb-1"
              Base
            ==
            ;input
              =type  "text"
              =id  "base"
              =name  "base"
              =required  ""
              =class  "w-full p-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
              ;
            ==
          ==
          ;div(class "flex items-center justify-center mt-6")
            ;button(type "submit", class "bg-blue-500 text-white font-bold py-2 px-6 rounded-lg hover:bg-blue-600 transition duration-300 ease-in-out")
              Make
            ==
            ;div(id "loading-indicator", class "ml-4 hidden")
              ;svg(class "animate-spin h-6 w-6 text-blue-500", xmlns "http://www.w3.org/2000/svg", fill "none", viewBox "0 0 24 24")
                ;circle(class "opacity-25", cx "12", cy "12", r "10", stroke "currentColor", stroke-width "4");
                ;path(class "opacity-75", fill "currentColor", d "M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z");
              ==
            ==
          ==
        ==
      ==
    ::
    ++  main-page
      |=  =cone:g
      ^-  manx
      ;html(lang "en")
        ;head
          ;meta(charset "UTF-8");
          ;meta(name "viewport", content "width=device-width, initial-scale=1.0");
          ;title: Grubbery
          ;script(src "https://cdn.tailwindcss.com");
          ;script(src "https://unpkg.com/htmx.org@1.9.4");
        ==
        ;body.p-4.flex.flex-col.justify-center.items-center.min-h-screen.bg-gray-100
          ;div
            ;*  %+  turn  ~(tap of cone)
                |=  [=path *]
                ;p: {(spud path)}
          ==
        ==
      ==
    ::
    ++  make-lib-page
      |=  pat=path
      =/  m  (charm ,manx)
      ^-  form:m
      ;<  [code=@t *]  bind:m  (peek-root-as ,[@t *] [%lib pat])
      ;<  =grub:g  bind:m  (peek-root [%bin pat])
      (pure:m (lib-page pat code grub))
    ::
    ++  lib-page
      |=  [=path code=@t =grub:g]
      ?>  ?=(%stem -.kind.grub)
      ^-  manx
      ;div(id (make-id [%lib path]))
        ;div.h-screen.p-4.flex.flex-col.justify-center.items-center.bg-gray-100
          ;div.max-h-screen.h-full.w-full.flex.flex-row
            ;+  (code-result [data tidy.kind]:grub)
            ;+  (textarea path (trip code))
          ==
        ==
      ==
    ::
    ++  code-result
      |=  [data=vase flag=? boom=(unit tang)]
      ^-  manx
      =/  color=tape  ?^(boom "red" ?.(flag "brown" "green"))
      ;div
        =class  "flex-1 flex flex-col w-full bg-{color}-200 p-8 rounded-lg shadow-lg h-full"
        ;div(class "text-xl font-bold mb-4 text-center text-{color}-700")
          ; {?:(flag "clean" "dirty")}
        ==
        ;*  ?^  boom
              ;=
                ;div(class "bg-{color}-100 p-4 rounded-lg w-full h-full overflow-y-auto")
                  ;code:"*{(render-tang-to-marl 80 u.boom)}"
                ==
              ==
            ?.  flag
              ~
            ;=
              ;div(class "bg-{color}-100 p-4 rounded-lg w-full h-full overflow-y-auto")
                ;code:"*{(render-tang-to-marl 80 (sell data) ~)}"
              ==
            ==
      ==
    ::
    ++  textarea
      |=  [=path contents=tape]
      ;form
        =hx-post  "/grub/make/lib"
        =hx-swap  "outerHTML"
        =hx-target  "#{(make-id [%lib path])}"
        =class  "flex-1 flex flex-col bg-white p-8 rounded-lg shadow-lg h-full"
        ;label
          =for  "textInput"
          =class  "block text-gray-700 font-bold mb-2"
          ; Enter your text
        ==
        ;input(type "hidden", name "path", value "{(spud path)}");
        ;textarea
          =id  "code"
          =name  "code"
          =rows  "6"
          =cols  "50"
          =class  "font-mono flex-grow h-full mb-3 w-full p-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
          =placeholder  "Type something..."
          ; {contents}
        ==
        ;br;
        ;button
          =type   "submit"
          =class  "w-full bg-blue-500 text-white font-bold py-2 px-4 rounded-lg hover:bg-blue-600 transition duration-300 ease-in-out"
          ; Submit
        ==
      ==
    ::
    ++  get-dom-manx
      |=  =path
      =/  m  (charm ,manx)
      ;<  =cone  bind:m  (peek /gui/boot/dom)
      ?~  grub=(~(get of cone) path)
        %-  pure:m
        ;div: No grub at this path.
      =/  res  (mule |.(!<(manx data.u.grub)))
      %-  pure:m
      ?-  -.res
        %&  p.res
        %|  ;div: Bad manx.
      ==
    ::
    ++  counter-page
      |=  [now=@da counter=manx is-even=manx parity=manx]
      ^-  manx
      ;html(lang "en")
        ;head
          ;meta(charset "UTF-8");
          ;meta(name "viewport", content "width=device-width, initial-scale=1.0");
          ;title: Counter App
          ;script(src "https://cdn.tailwindcss.com");
          ;script(src "https://unpkg.com/htmx.org@1.9.4");
        ==
        ;body.p-4.flex.flex-col.justify-center.items-center.min-h-screen.bg-gray-100
          ;+  (refresher-component now ~)
          ;+  counter
          ;+  is-even
          ;+  parity
        ==
      ==
    --
  --
::
++  counter
  %-  crip
  """
  /-  t  /add/two
  =,  grubberyio
  |=  [=bowl:base:g =stud:g =vase]
  =/  m  (charm:base:g ,pail:g)
  ^-  form:m
  ?+    stud  !!
      [%inc ~]
    ;<  a=@ud  bind:m  (get-state-as @ud)
    (pour !>(+(a)))
    ::
      [%two ~]
    ;<  a=@ud  bind:m  (get-state-as @ud)
    (pour !>((two:t a)))
  ==
  """
::
++  is-even
  %-  crip
  """
  |=  =bowl:stem:g
  :-  ~
  =+  !<(=@ud (~(got by deps.bowl) /counter))
  !>(=(0 (mod ud 2)))
  """
::
++  parity
  %-  crip
  """
  |=  =bowl:stem:g
  :-  ~
  ?:  !<(? (~(got by deps.bowl) /is-even))
    !>('true')
  !>('false')
  """
::
++  add-two
  %-  crip
  """
  |%
  ++  two  |=(a=@ud (add 2 a))
  --
  """
--
