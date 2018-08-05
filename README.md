# ina

## goals and intended benefits of this platform

* mostly host-platform independent
  * run inside a common browser or run natively under a common OS
* high level universal data format for everything
  * all data is accessible and easily processed
  * all programs can easily communicate with each other
* capability secure
  * all programs are safe to run by default
* "programming" is a primary form of interaction
  * evaluate anything, anywhere, at any time, and get immediate feedback
  * computation is lightweight: great control over resource consumption
* first class sharing and distributed workspaces
* self-contained packaging/migration of any subset of data/platform
* step outward into the metasystem at any time
  * universal undo/redo beyond any "version control" system
  * redesign platform and tools to improve, repurpose, or specialize them
* everything is "version controlled" and all dependencies are tracked
  * there aren't really versions, just alternatives that can coexist
  * persistent dependency resolution (no hell, no bitrot, no "upgrade regret")
    * things that you depend on are never updated/replaced/removed implicitly
    * potential dependency changes are recognized, can be propagated expicitly
      * recognition may be due to provenance and/or pattern matching
    * safe to speculatively change anything, the original is not destroyed
      * dependency resolution changes can always be rolled back
  * provenance and explanations
    * when/how was this data computed?
    * what other data participated and where did it come from?
    * why is this value what it is?
    * what would happen if X was changed to Y?
    * what would have to happen for X to change to Y?
