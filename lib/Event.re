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
