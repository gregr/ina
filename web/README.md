# web

## TODO

### semi-isolated embedded interactive systems

Normally, when evaluating programs within the editor, the results are displayed as structured data (that can then also be manipulated).  This data can be visualized as raw symbolic expressions, or with custom visualizations (e.g., viewing the data in some graphical form, or with a different notation that better matches the domain).

Beyond treating results as data, some can also be treated as interactive systems of their own (the editor itself can be thought of as such an interactive system).  For safety by default, these interactions are isolated/sandboxed, in that any events/effects they have are recorded in buffers, but don't otherwise escape.  If desired, their effects/events can be hooked up to capabilities that allow them to communicate with and manipulate external systems, including the host editor.

These embedded interactive systems should support logging, replaying, and arbitrary splicing/editing of incoming and generated events (event sourcing), to support exploration and debugging.

Bootstrapping goal: it should be possible to build new systems, such as games or editors, as embedded interactions for testing, and then reboot directly into the interaction to treat it as the new host system (shedding/garbage collecting any "dead code or data" not needed from the old host system).


### self-extensible browser-based editor

* text-like editor mode
  * present an AST-editing interface that feels like text editing
  * how to move subterms to remote locations?
    * name-based contexts
    * how to close terms? is it even necessary anymore? probably only upon extraction, which shouldn't be a problem
  * left/right arrow keys navigate per-char; up/down quickly jump parent/child expressions, remembering last-visited child?
    * shift+arrow for larger movements
    * some way to toggle/reorder siblings, sort of like reversing a subrange of widgets
  * unit acts like a typing context, space opens a new such context
  * typing the first character in a new context should clearly indicate its syntactic category
  * numeric characters do the usual
  * alphabetic characters start a new variable
    * only prefixes for in-scope vars will work (open up an auto-complete list)
    * should have some way to start variable for non-alpha chars
  * { opens new pair; } leaves it
  * ( opens a new application; ) in the final context leaves it
  * \ begins a lambda, moving into the arg list (while showing "(lambda (")
    * ) leaves arg list; ) again leaves lam body
    * new contexts inside the arg list are like lam-wrapping
    * removing var applies that lam to unit


### smart editors
* non-textual
  * directly manipulate program structure
    * syntactically-correct programs by construction
    * non-textual modes of input and presentation possible
  * removes bias towards monolithic text files
    * many independently-developed code fragments grouped and combined in multiple ways
* context-aware editing and exploration
  * code completion/suggestions
  * "time-traveling" symbolic evaluation and debugging; better than a REPL
  * programmable state/output visualization and dependency/correlation tracing
    * which portions of the program determine a given component of the output?
  * generalization from examples
  * allows more intelligent version control
  * interact/inspect/debug under lambda: abstract bindings
  * debugging with provenance tracking
* layered program views and elaboration
  * infer lower levels from higher levels
    * specifications -> high-level implementations -> low-level representations
  * semi-manually intervene/optimize at any level
* programmable visualization and interaction
  * produce specialized WYSIWYG "editors" for creative tasks
* collaboration tools
  * simultaneous manipulation of shared spaces with other users
  * remote space/repository/DB publish/subscribe
* conveniently ubiquitous programming via html-embedded editor widgets
  * widget allows page-local manipulation/execution
  * can be rendered chrome-less (for seamless appearance) or expanded into the full editor in-page
  * can connect to externally hosted DBs to import/export additional programs/content
    * browsing a page with embedded code widgets and want to ...
      * ... interact with the embedded code later?
        * export a reference/copy to your personal DB
      * ... interact with the embedded code while incorporating some of your own code?
        * immediately import it from your personal DB
    * on top of serendipitous programming, enables working on personal projects from anywhere
      * assumes you trust the machine you run the browser on, or provide some other security mechanism
        * such as accessing revoke-able and/or read-only DB capabilities
  * editor can support more than just "code" editing
    * symbol manipulation (typical programming), graphics, sound, arbitrary html, other domain-specific editing
      * arbitrary html would mean you can run the editor in the editor...
      * similar ideas? http://blog.duangle.com/2015/01/conspire-programming-environment-for.html


### Visualization

* text
  * tokens
  * labels
  * input boxes
* UI elements
  * boolean
    * checkbox, radio, dropdown/list, lightswitch
  * numbers
    * slider, color selector
    * textual digits in different bases: 7 vs. #b111
  * tuples and sets
    * (labeled) lists/forms
    * tables
* graphics
  * numbers
    * stack/line/bundle of bullets/boxes/sticks
    * polygons
    * circular/rectangular/bar areas
    * points on a scale
    * scaled lines
    * colors
  * lists
    * numerical
      * cartesian/polar plots
      * vectors
      * areas, angular sweeps
      * images
    * general
      * juxtapose, superimpose
      * deltas, paths, motion/animation
  * sets
    * statistics and summaries
      * brackets/ranges, candlesticks
      * histograms


### Data representation

Annotating data with metadata supports choosing UI that depends on context.

* metadata ideas
  * uids
  * UI-related concerns such as element styling/layout/navigation-modes
  * provenance tracking
  * aspect info based on usage, such as syntax highlighting annotations for data treated as code
    * e.g., keyword, variable, literal, expr/component/grouping with type/arity
    * this information guides/augments editing/visualization interfaces
      * e.g., variable renaming, subexpr choices/constraints, syntax errors, rich literal editors/renderers
      * debuggers via hyperprogrammed small step evaluators
    * "parsing" code maps uids to corresponding aspect info
* mirror tree for annotating data with metadata
  * keep data structures separately interpretable in O(1) from their metadata


### Misc design and UI ideas

* code layout/styling/navigation
  * configure absolute vs. hierarchical navigation per element
  * style information as data that's also manipulated in the editor
  * default vs. override
  * layout, direction
  * lanky, compact
  * styling for quote, quasiquote, unquote
    * generally, styling to preserve presentation close to original text
* highlight/animate changes
* clearly indicate UI element selection/selectability
* map simple data to document types?
  * visual indication
  * lists may change presentation to look like underlying pairs in hierarchical mode
  * absolute mode may force alignment with adjacent siblings along structural similarity
    * spreadsheet-like grid
* folder-like exploration
  * scopes and palettes
  * garbage area
* allow user to provide alternative UI and data values for cells
  * allows exploring alternate computations by just clicking a radio button, check box, etc.
* maybe infer lenses from editing actions
* possible sources of inspiration:
  * https://semantic-ui.com/
  * https://github.com/Shopify/draggable
  * https://conclave-team.github.io/conclave-site/
  * https://docs.google.com/presentation/d/1MD-CgzODFWzdpnYXr8bEgysfDmb8PDV6iCAjH5JIvaI/edit#slide=id.g1d7c11dd1b_0_304


### Example apps

* kill the bill: splitting a bill via collaborative editing
