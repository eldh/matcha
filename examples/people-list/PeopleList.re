open Matcha;

/* Simulate fetching favorite color from API */
let fetchFavoriteColor = (name: string): string => {
  /* Simulate different colors for different people */
  switch (name) {
  | "Alice" => "Blue"
  | "Bob" => "Green"
  | "Charlie" => "Red"
  | "Diana" => "Purple"
  | "Eve" => "Orange"
  | _ => "Gray"
  };
};

[@component]
let make = (~people: array(Types.person)) => {
  /* Get terminal dimensions from context */
  let {TerminalContext.width, height} = TerminalContext.use();
  let quit = Event.useQuit();
  let (activePerson, setActivePerson) = Component.useState(None);
  let (favoriteColor, setFavoriteColor) = Component.useState(None);
  let (isLoading, setIsLoading) = Component.useState(false);

  /* Get the active person's name for dependency tracking */
  let activePersonName =
    switch (activePerson) {
    | Some(p) => p.Types.name
    | None => ""
    };

  /* Fetch favorite color when active person changes */
  Hooks.useEffect(
    () => {
      switch (activePerson) {
      | None =>
        setFavoriteColor(None);
        setIsLoading(false);
        None; /* No cleanup needed */
      | Some(person) =>
        setIsLoading(true);
        setFavoriteColor(None);

        /* Cancellation token */
        let cancelled = ref(false);

        /* Simulate API delay with a thread */
        let _ =
          Thread.create(
            () => {
              Thread.delay(0.5); /* 500ms delay */
              /* Only update state if not cancelled */
              if (! cancelled^) {
                let color = fetchFavoriteColor(person.name);
                setFavoriteColor(Some(color));
                setIsLoading(false);
              };
            },
            (),
          );

        /* Return cleanup function that cancels the fetch */
        Some(() => {cancelled := true});
      }
    },
    [|activePersonName|],
  );

  let handleSelect = (_person: Types.person) => {
    quit();
  };

  let handleActive = (person: option(Types.person)) => {
    setActivePerson(person);
  };

  let renderPerson = (person: Types.person, isSelected) =>
    <Person name={person.name} age={person.age} selected=isSelected />;

  let getFilterText = (person: Types.person) => person.name;

  let leftPane =
    <Column>
      <Dim>
        <Text> "People (↑/↓ navigate, Enter select, Q quit)\n" </Text>
      </Dim>
      <FilterableList
        items=people
        renderItem=renderPerson
        onSelect=handleSelect
        onActive=handleActive
        getFilterText
      />
    </Column>;

  let colorDisplay =
    if (isLoading) {
      <Dim> <Text> "Loading favorite color..." </Text> </Dim>;
    } else {
      switch (favoriteColor) {
      | None => Element.empty
      | Some(color) => <Text> {"Favorite color: " ++ color} </Text>
      };
    };

  let rightPane =
    switch (activePerson) {
    | None => <Dim> <Text> "No person selected" </Text> </Dim>
    | Some(person) =>
      <Column>
        <Bold> <Text> {person.name} </Text> </Bold>
        <Text>
          {"\nAge: " ++ string_of_int(person.age) ++ " years old\n"}
        </Text>
        colorDisplay
        <Dim>
          <Text>
            {"\n\nTerminal: "
             ++ string_of_int(width)
             ++ "x"
             ++ string_of_int(height)}
          </Text>
        </Dim>
      </Column>
    };

  <SplitView
    left=leftPane
    right=rightPane
    leftWidth={Some(SplitView.Percent(40))}
    width
    height
    padding=None
  />;
};
