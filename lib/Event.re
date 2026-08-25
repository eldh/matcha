/*
 * Event - Convenience re-exports for event handling hooks
 *
 * This module provides easy access to event-related hooks.
 * Import this module to handle keyboard events and app lifecycle.
 */

/* Register a keyboard event handler.
 * See Hooks.useKeyDown for full documentation.
 */
let useKeyDown = Hooks.useKeyDown;

/* Get a function to quit the application.
 * See Hooks.useQuit for full documentation.
 */
let useQuit = Hooks.useQuit;

/* Focus (B1): claim a focus id, imperatively move focus, and gate a key
 * handler on being focused. See Hooks.useFocus/useFocusManager/useInput for
 * full documentation.
 */
let useFocus = Hooks.useFocus;
let useFocusManager = Hooks.useFocusManager;
let useInput = Hooks.useInput;

/* Mouse (B4): register a handler for mouse events hitting this component's
 * rendered box. See Hooks.useMouse. */
let useMouse = Hooks.useMouse;
