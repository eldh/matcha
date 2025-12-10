open Rere;

[@component]
let make = (~people: array(Types.person)) => {
  /* Get terminal dimensions from context */
  let {TerminalContext.width, height} = TerminalContext.use();
  let quit = Event.useQuit();
  let (activePerson, setActivePerson) = Component.useState(None);

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

  let rightPane =
    switch (activePerson) {
    | None => <Dim> <Text> "No person selected" </Text> </Dim>
    | Some(person) =>
      <Column>
        <Bold> <Text> {person.name} </Text> </Bold>
        <Text>
          {"\nAge: " ++ string_of_int(person.age) ++ " years old"}
        </Text>
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
