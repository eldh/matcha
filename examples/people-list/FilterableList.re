open Matcha;

/* Check if haystack contains needle as a substring */
let stringContains = (haystack, needle) => {
  let needleLen = String.length(needle);
  let haystackLen = String.length(haystack);
  if (needleLen > haystackLen) {
    false;
  } else if (needleLen == 0) {
    true;
  } else {
    let found = ref(false);
    for (i in 0 to haystackLen - needleLen) {
      if (String.sub(haystack, i, needleLen) == needle) {
        found := true;
      };
    };
    found^;
  };
};

/* Delete the last word from a string (Ctrl+W behavior) */
let deleteLastWord = (s: string): string => {
  let len = String.length(s);
  if (len == 0) {
    "";
  } else {
    /* First, skip trailing spaces */
    let rec skipSpaces = i =>
      if (i < 0) {
        0;
      } else if (s.[i] == ' ') {
        skipSpaces(i - 1);
      } else {
        i;
      };

    /* Then, skip the word characters */
    let rec skipWord = i =>
      if (i < 0) {
        0;
      } else if (s.[i] != ' ') {
        skipWord(i - 1);
      } else {
        i + 1;
      };

    let afterSpaces = skipSpaces(len - 1);
    let wordStart = skipWord(afterSpaces);

    if (wordStart <= 0) {
      "";
    } else {
      String.sub(s, 0, wordStart);
    };
  };
};

let filterItems = (items, getFilterText, filter) =>
  if (String.length(filter) == 0) {
    items;
  } else {
    let filterLower = String.lowercase_ascii(filter);
    items
    |> Array.to_list
    |> List.filter(item => {
         let text = String.lowercase_ascii(getFilterText(item));
         stringContains(text, filterLower);
       })
    |> Array.of_list;
  };

[@component]
let make =
    (
      ~items: array('a),
      ~renderItem: ('a, bool) => Element.t,
      ~onSelect: 'a => unit,
      ~onActive: option('a) => unit,
      ~getFilterText: 'a => string,
    ) => {
  let (filter, setFilter) = Component.useState("");
  let (selectedIndex, setSelectedIndex) = Component.useState(0);

  let filteredItems = filterItems(items, getFilterText, filter);
  let maxIndex = max(0, Array.length(filteredItems) - 1);

  /* Clamp selected index when filtered results change */
  let currentIndex = min(selectedIndex, maxIndex);

  /* Notify parent of active item */
  let activeItem =
    if (Array.length(filteredItems) > 0) {
      Some(filteredItems[currentIndex]);
    } else {
      None;
    };
  onActive(activeItem);

  Event.useKeyDown((key, modifiers) => {
    switch (key, modifiers) {
    | (Key.Arrow_up, _) => setSelectedIndex(max(0, currentIndex - 1))
    | (Key.Arrow_down, _) =>
      setSelectedIndex(min(maxIndex, currentIndex + 1))
    | (Key.Enter, _) =>
      if (Array.length(filteredItems) > 0) {
        onSelect(filteredItems[currentIndex]);
      }
    | (Key.Backspace, _) =>
      if (String.length(filter) > 0) {
        setFilter(String.sub(filter, 0, String.length(filter) - 1));
        setSelectedIndex(0);
      }
    | (Key.KillLine, _) =>
      /* Ctrl+U clears the entire filter (standard terminal shortcut) */
      setFilter("");
      setSelectedIndex(0);
    | (Key.KillWord, _) =>
      /* Ctrl+W deletes the last word */
      setFilter(deleteLastWord(filter));
      setSelectedIndex(0);
    | (Key.Delete, _) =>
      /* Delete key also clears filter (alternative to backspace) */
      if (String.length(filter) > 0) {
        setFilter(String.sub(filter, 0, String.length(filter) - 1));
        setSelectedIndex(0);
      }
    | (Key.Char(c), {Key.ctrl: false, alt: false, _}) =>
      setFilter(filter ++ String.make(1, c));
      setSelectedIndex(0);
    | _ => ()
    }
  });

  let itemElements =
    filteredItems
    |> Array.mapi((i, item) => renderItem(item, i == currentIndex))
    |> Array.to_list;

  if (Array.length(filteredItems) == 0) {
    /* No matches - show filter info */
    let noMatchText =
      if (String.length(filter) > 0) {
        "No matches for \"" ++ filter ++ "\"";
      } else {
        "No items";
      };
    <Text dim=true> noMatchText </Text>;
  } else {
    <Column> itemElements </Column>;
  };
};
