# web

## TODO

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
