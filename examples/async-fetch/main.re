/*
 * Async Fetch Example
 *
 * Demonstrates that background thread state updates correctly wake the main loop.
 * This example simulates fetching data from an API using a background thread.
 *
 * Press 'f' to start a simulated fetch - the UI should update immediately
 * when the "fetch" completes, without waiting for keyboard input.
 */

open Matcha;

/* Simulated data that would come from an API */
type fetchResult = {
  id: int,
  title: string,
  timestamp: float,
};

/* State for the fetch operation */
type fetchState =
  | Idle
  | Loading
  | Success(fetchResult)
  | Error(string);

/* Counter for generating unique IDs */
let fetchCounter = ref(0);

[@component]
let make = () => {
  let quit = Event.useQuit();
  let (fetchState, setFetchState) = Component.useState(Idle);
  let (fetchCount, setFetchCount) = Component.useState(0);

  /* Simulate an async fetch operation in a background thread */
  let startFetch = () => {
    setFetchState(Loading);

    /* Spawn a background thread to simulate async work */
    let _ =
      Thread.create(
        () => {
          /* Simulate network latency (1-3 seconds) */
          let delay = 1.0 +. Random.float(2.0);
          Thread.delay(delay);

          /* Simulate occasional failures */
          if (Random.int(5) == 0) {
            setFetchState(Error("Network error: connection timed out"));
          } else {
            fetchCounter := fetchCounter^ + 1;
            let result = {
              id: fetchCounter^,
              title: "Fetched Item #" ++ string_of_int(fetchCounter^),
              timestamp: Unix.gettimeofday(),
            };
            setFetchState(Success(result));
            setFetchCount(fetchCount + 1);
          };
        },
        (),
      );
    ();
  };

  Event.useKeyDown((key, modifiers) => {
    switch (key, modifiers) {
    | (Key.Char('q'), _)
    | (Key.Char('Q'), _) => quit(ClearScreen)
    | (Key.Char('c'), {Key.ctrl: true, _}) => quit(ClearScreen)
    | (Key.Char('f'), _)
    | (Key.Char('F'), _) =>
      switch (fetchState) {
      | Loading => () /* Don't start another fetch while loading */
      | _ => startFetch()
      }
    | (Key.Char('r'), _)
    | (Key.Char('R'), _) => setFetchState(Idle)
    | _ => ()
    }
  });

  /* Format timestamp */
  let formatTime = (t: float): string => {
    let tm = Unix.localtime(t);
    Printf.sprintf(
      "%02d:%02d:%02d",
      tm.Unix.tm_hour,
      tm.Unix.tm_min,
      tm.Unix.tm_sec,
    );
  };

  /* Render the current state */
  let stateDisplay =
    switch (fetchState) {
    | Idle => <Text dim=true> "No data fetched yet. Press 'f' to fetch." </Text>
    | Loading =>
      <HStack>
        <Text bold=true color=Yellow> "⏳ " </Text>
        <Text> "Fetching data..." </Text>
      </HStack>
    | Success(result) =>
      <VStack>
        <HStack>
          <Text bold=true color=Green> "✓ " </Text>
          <Text> "Fetch successful!" </Text>
        </HStack>
        <Text> "" </Text>
        <HStack>
          <Text dim=true> "  ID: " </Text>
          <Text> {string_of_int(result.id)} </Text>
        </HStack>
        <HStack>
          <Text dim=true> "  Title: " </Text>
          <Text bold=true> {result.title} </Text>
        </HStack>
        <HStack>
          <Text dim=true> "  Time: " </Text>
          <Text> {formatTime(result.timestamp)} </Text>
        </HStack>
      </VStack>
    | Error(msg) =>
      <VStack>
        <HStack>
          <Text bold=true color=Red> "✗ " </Text>
          <Text color=Red> "Fetch failed!" </Text>
        </HStack>
        <Text dim=true> {"  " ++ msg} </Text>
      </VStack>
    };

  <VStack>
    <Text bold=true color=Cyan> "Async Fetch Example" </Text>
    <Text dim=true>
      "Demonstrates background thread state updates waking the main loop"
    </Text>
    <Text> "" </Text>
    <Text> "─────────────────────────────────────────────────" </Text>
    <Text> "" </Text>
    stateDisplay
    <Text> "" </Text>
    <Text> "─────────────────────────────────────────────────" </Text>
    <Text> "" </Text>
    <HStack>
      <Text dim=true> "Total successful fetches: " </Text>
      <Text bold=true> {string_of_int(fetchCount)} </Text>
    </HStack>
    <Text> "" </Text>
    <Text dim=true> "f: Fetch data  r: Reset  q: Quit" </Text>
    <Text> "" </Text>
    <Text dim=true italic=true>
      "Note: UI updates immediately when fetch completes,"
    </Text>
    <Text dim=true italic=true>
      "without waiting for keyboard input."
    </Text>
  </VStack>;
};

module App = {
  let make = make;
};

let () = {
  /* Initialize random seed for varied delays */
  Random.self_init();
  Runtime.start((module App));
};
