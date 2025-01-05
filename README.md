## What is Piping?
Piping is a small library providing mechanisms to create message passing pipeline constructs.

## How To
Load Piping through Quicklisp or ASDF:

```
(ql:quickload :piping)
```

Create a pipeline and add some pipe segments:

```
(defvar *pipeline* (make-instance 'pipeline))
(add-segment *pipeline* (make-instance 'predicate-filter :predicate #'evenp))
(add-segment *pipeline* (make-instance 'printer))
```

Using pass you can then pass items down the pipeline:

```
(pass *pipeline* 7)
(pass *pipeline* 6)
```

Often times you'll want to create pipe branches so to speak, to split off passing and to filter for different things:

```
(add-segment *pipeline* (make-pipe))
(add-segment *pipeline* (make-instance 'predicate-filter :predicate #'zerop) '(2))
(add-segment *pipeline* (make-instance 'printer) '(2))
```

The last argument to `add-segment` is a place indicator. A place is always a list of indexes, each index specifying the index within the current pipe. `add-segment` then adds the segment to the end of the pipe, in this case the pipe we added.

In order to have easier access to places, you can use `set-name` to create names for certain places. Do note that modifying the pipeline might change or destroy names. See the each function's docstring for more information.

Creating custom pipe segments is just a question of subclassing `segment` and defining the `pass` method. If your segment should act like a pipe, you'll have to define methods for `find-place`, `find-parent`, `insert` and `withdraw` as well.

Visualizing the pipeline can be done relatively well by simply printing the main pipe:

```
(pipeline *pipeline*)
; ==> #(:FILTER: &gt;&gt;FAUCET #(:FILTER: &gt;&gt;FAUCET)
```
