#ifndef _H_IEGUI

  #define _H_IEGUI

  #command DEFINE BROWSER <Browser>;
           [ <addr: ADDRESSBAR> ];
           [ <tool: TOOLBAR> ];
           [ <status: STATUSBAR> ];
           [ <theater: THEATER> ];
        => ;
           <Browser> := IE_Browser( <.addr.>, <.tool.>, <.status.>, <.theater.> )

  #command DEFINE FORM <FormID> OF <Browser> [AS <Var>];
           [TITLE <Title>];
           [BODY <Body>];
           [OPEN <Url>];
           [ON <EventN> DO <ActionN>];
           [BEHAVIORS <Behaviors,...>];
        => ;
           [ <Var> := ] IE_Form( <(FormID)>, <Browser>, <(Body)>, <(Url)>, <(Title)>, { "" => NIL [, <(EventN)> => {|oSource| (oSource), <ActionN> } ] }, {<(Behaviors)>} )

  #command REDEFINE GET <ElementID> OF <Form> VAR <var> [AS <oVar>] ;
           [VALID <valid>];
           [WHEN <when>];
           [PICTURE <pic>];
           [ON <EventN> DO <ActionN>];
           [BEHAVIORS <Behaviors,...>];
        => ;
           [ <oVar> := ] IE_RedefineGet( <Form>, <(ElementID)>, GetList, _GET_( <var>, <"var">, <pic>, <{valid}>, <{when}> ), { "" => NIL [, <(EventN)> => {|oSource| (oSource), <ActionN> } ] }, {<(Behaviors)>} )

  #command DEFINE GET <ElementID> OF <Form> VAR <var> [AS <oVar>];
           [SIZE <size>];
           [VALID <valid>];
           [WHEN <when>];
           [PICTURE <pic>];
           [BEFORE <Before>];
           [ON <EventN> DO <ActionN>];
           [BEHAVIORS <Behaviors,...>];
        => ;
           [ <oVar> := ] IE_DefineGet( <Form>, <(ElementID)>, <(Before)>, NIL , NIL , <size>, GetList, _GET_( <var>, <"var">, <pic>, <{valid}>, <{when}> ), { "" => NIL [, <(EventN)> => {|oSource| (oSource), <ActionN> } ] }, {<(Behaviors)>} )

  #command @ <top>, <left> DEFINE GET <ElementID> OF <Form> VAR <var> [AS <oVar>];
           [SIZE <size>];
           [VALID <valid>];
           [WHEN <when>];
           [PICTURE <pic>];
           [BEFORE <Before>];
           [ON <EventN> DO <ActionN>];
           [BEHAVIORS <Behaviors,...>];
        => ;
           [ <oVar> := ] IE_DefineGet( <Form>, <(ElementID)>, <(Before)>, <top>, <left>, <size>, GetList, _GET_( <var>, <"var">, <pic>, <{valid}>, <{when}> ), { "" => NIL [, <(EventN)> => {|oSource| (oSource), <ActionN> } ] }, {<(Behaviors)>} )

  #command @ <top>, <left> DEFINE BUTTON <ElementID> OF <Form> [AS <oVar>];
           [SIZE <size>];
           [BEFORE <Before>];
           [ON <EventN> DO <ActionN>];
           [BEHAVIORS <Behaviors,...>];
        => ;
           [ <oVar> := ] IE_DefineButton( <Form>, <(ElementID)>, <(Before)>, <top>, <left>, <size>, { "" => NIL [, <(EventN)> => {|oSource| (oSource), <ActionN> } ] }, {<(Behaviors)>} )

  #command REDEFINE <ElementID> OF <Form> [AS <oVar>] ;
           [ON <EventN> DO <ActionN>];
           [BEHAVIORS <Behaviors,...>];
        => ;
           [ <oVar> := ] IE_RedefineElement( <Form>, <(ElementID)>, { "" => NIL [, <(EventN)> => {|oSource| (oSource), <ActionN> } ] }, {<(Behaviors)>} )

  #command DEFINE BEHAVIOR <Behavior>;
           [ON <EventN> DO <ActionN>];
        => ;
           IE_DefineBehavior( <(Behavior)>, { "" => NIL [, <(EventN)> => {|oSource| (oSource), <ActionN> } ] } )


/*
  #command ACTIVATE BROWSER <Browser> => IE_Activate( <Browser> )

  #command ACTIVATE FORM ID <FormID> OF <Browser> MODAL;
        => ;
           IE_ModalForm( <(FormID)>, <Browser>, GetList )

  #command ACTIVATE FORM ID <FormID> OF <Browser>;
        => ;
           IE_NonModalForm( <(FormID)>, <Browser>, GetList )
*/

  #command ACTIVATE FORM <Form> MODAL;
        => ;
           IE_ModalForm( <Form>, NIL, GetList )

  #command ACTIVATE FORM <Form>;
        => ;
           IE_NonModalForm( <Form>, NIL, GetList )

  #command CLOSE FORM <Form> => IE_CloseForm( @<Form> )

  #command CLOSE BROWSER <Browser> => IE_CloseBrowser( @<Browser> )

#endif