/*
 * The launcher for examples/command-menu. The app itself lives in
 * CommandMenuApp.re, as a library, so that test/commandmenu_tests.re can
 * drive the very same component headlessly - see the header comment there.
 *
 * WHY FULLSCREEN, AND THE RULE IT ILLUSTRATES
 * -------------------------------------------
 * This app fills the screen: its body is <Sized size={Flex(1)}>, so the
 * frame is exactly as tall as the terminal.
 *
 * An INLINE app paints its live region AT THE CURSOR. If that region is as
 * tall as the terminal, the terminal has to SCROLL to make room, which
 * pushes whatever was above it - your prompt, your last command - up and
 * away. Quitting erases the region cleanly, but nothing can un-scroll a
 * terminal, so you are left staring at a screen of blank rows with your
 * previous command far above them. That is not a bug in the erase; it is
 * an app that asked for the whole screen while rendering inline.
 *
 * The rule: AN INLINE APP MUST BE SHORTER THAN THE TERMINAL. If your root
 * Flexes to fill the screen, you want ~screen=Fullscreen - the alternate
 * screen, which the terminal restores exactly on exit, leaving the prompt
 * where it was. examples/chat and examples/static-demo are the compact
 * inline shape; this and examples/claude-code are the fullscreen one.
 *
 * Fullscreen costs this app nothing: it uses neither <Static> nor
 * useStdout (both of which raise there, since the alternate screen has no
 * scrollback to commit to), and the palette composites over the live log
 * exactly the same way in either mode.
 */
let () =
  Matcha.Runtime.start(~screen=Fullscreen, (module CommandMenuApp.App));
