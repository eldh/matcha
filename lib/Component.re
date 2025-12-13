/*
 * Component - Convenience re-exports for component authoring
 * 
 * This module provides easy access to commonly used hooks when
 * building components. Import this for state and context access.
 */

/* Create local state for a component.
 * See Hooks.useState for full documentation.
 */
let useState = Hooks.useState;

/* Read the current value of a context.
 * See Context.use for full documentation.
 */
let useContext = Context.use;
