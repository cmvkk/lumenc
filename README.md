Lumenc - A Clojure framework for generating music.
==================================================

Running: 
--------

Open the 'lumenc' file and edit the CLOJURE variable to point to your copy of clojure.jar.  Then you can simply do

    ./lumenc your_source_file.clj

and the resultant .wav will be created in your current directory.

Notes:
------

This software should be considered pre-pre-alpha.  Although I'll try to keep commits such that the demos always work, the software is likely buggy and certainly the range of filters available right now leaves much to be desired.  Part of the reason I'm doing this is to learn about digital audio synthesis, so the framework provides a platform from which I can test concepts as I learn them.  

It should also be pointed out that the software is very slooooooow.  Often several times slower than real time for even the simplest of tracks.  This is for a few reasons.  The first is that Clojure itself boxes numbers between function calls, so the nature of the chained-closure setup here makes mathematical operations much slower than they might be in plain java, even when using unchecked math which is sometimes possible.  Secondly, many of the filters written herein are written with an emphasis on readability rather than speed.  My goals for this project include putting readability and 'tweakability' of the filters as the first priority, making it easy to use and extend the available filters.  Because the system is not required to run in real time, speed will always take second place to this.  I do hope to optimise whenever possible though.

A Quick Overview:
-----------------

Lumenc provides two main data structures for creating music, waves and tracks.  Waves are digital representations of PCM data -- a series of samples.  Tracks are representations of melodies. 

### Waves

Waves are generally created by calling filters, which are just functions that return waves.  `sine` is an example, and `(sine 440)` returns a wave that represents a sine wave at 440Hz.  Other filters take waves as arguments as well, for example `(gain (sine 440) 0.5)` produces a similar sine wave at half amplitude.  Many filters take waves for their options, including sine, so for example `(sine (stretch (sine 2) 440 660))` produces a sine wave that modulates in frequency between 440Hz and 660Hz twice a second.  

### Rendering

Waves can be written out to file using the `render` macro.  For example `(render ["out.wav" (secs 10)] (sine 440))` writes ten seconds of a 440Hz sine wave out to the file "out.wav" in the directory where you ran the script.

### The `wave` Macro

This macro allows you to define a 'primitive' wave, using clojure code.  It takes an argument list like `[xs :and bindings]` where xs are any number of input waves, and bindings are bindings of anything.  Inside the body, the waves will evaluate to the 'current sample' of that wave.  You can then return the sample value for your wave with the form `(give ret-val rebindings)` where ret-val is the value for that sample, and rebindings are new bindings for the next sample.  A wave that halves the amplitude of an input wave might look like this: `(wave [input] (give (/ input 2)))`.  

### Rstack

This is a macro that provides the ability to connect filters together in a feedback loop.  It takes a series of label/filter pairs, and as a result provides those labels to be used as input waves to other filters in the rstack, either before or after.  In order for this to work correctly, at least one of the filters has to be a 'buffer' or 'shift' filter, so as to keep infinite loops from occuring.  An example might be `(rstack x (sine 440) y (buffer z 100) z (add x y))`, which would produce a feed-back echo of the sine wave every 100 seconds.

### Filters

Filters are basically just functions, and you can define them as such.  The macro `deffilter` is also available, though currently it is functionally equivalent to `defn`.  For example: `(deffilter quiet [input] (wave [input] (give (/ input 2))))` can then be used to create a wave, `(quiet (sine 440))`.  

### Tracks

Tracks are representations of a modulating value over time.  They're generally used to represent melodies, drum beats, varying volumes, etc.  They're created using `deftrack`, for example `(deftrack foo [c d e f g a b c])` which creates a track 'foo' that represents a C major scale.  When a track is passed to a filter that expects a wave, that track will be coerced into wave form.  For example, `(sine foo)` would produce a sine wave playing the C major scale. 

### Deftrack

Deftrack is a macro that creates one or more tracks.  It takes any number of label*/form* pairs, where label* can be any number of labels, and form* can be any number of forms.  The resultant tracks represent all forms in the pair applied to each label in the pair, in order.  The forms are of two types.  Vectors (as above) represent real value information, while maps contain type info for each track.  Within the vectors, each element represents a length of one beat (defaultly).  In the above example, each note would be played for one beat worth of time.  The length of a beat can be changed by wrapping your render call in the `with-bpm` macro, which takes a number representing beats per minute.  The elements inside a track vector can be either symbols, representing a single value, or lists, represnting a series of values.  For each list, the notes inside are split evenly amongst the time available.  so for example `(deftrack bar [a b (c d) (e (f g))])`, 'a' and 'b' each are one beat long, 'c', 'd', and 'e' are half a beat long, and 'f' and 'g' are each a quarter beat long.  There are two special values, '.' and '*'.  '.' represents a rest, and produces silence.  '*' represents a hold, and just continues the previous note uninterrupted.  Therefore `[a *]` represents one note, two beats long.  

### Track maps

Deftrack also accepts maps as values, allowing you to set options for each track.  The main attribute of tracks is :type.  This takes a vector of type keywords.  Other keywords allow for type-specific attributes.  For example `(deftrack foo {:type [:raw]} [1 2 3])` produces a track that modulates across the values 1 2 and 3.

### Note tracks

The default track type is :note.  This type automatically takes note symbols and converts them into frequencies that can then be passed to a wave.  A note symbol follows this format: letter[octave][fsn][direction+], where 'letter' is a note letter (a through g), octave is an optional number between 1 and 8, representing the current octave (octave 4 starts with middle C), direction is any number of + or - signs, representing the direction of the note, and fsn is either 'b', '#', or 'n' representing a flat, sharp, or natural.  If an octave number isn't provided, the note's octave will simply be the one that makes it closest to the previous note.  As a result, for typical melodies it isn't necessary to include any octave information at all.  The direction markers give some guidance on this part.  Using a +, for example, will produce the note above the previous note, while a - will produce the note below the previous note.  Extra + and - signs represent octave jumps in those directions. 

### Other track types

As seen earlier, :raw tracks simply return the values in the track without changing them at all.  Tracks of type :option have an attribute :opts, which takes a map.  The keys of this map are then used in the track itself, and those keys are then converted to the map's corresponding values when the track is processed.  This is useful if you want to modulate between a small number of large values.   :arpeggio tracks allow vectors of values, transforming those vectors into arpeggios of the values inside.  They take an option :alen which represents the beat length of each note inside the arpeggio.  

### Creating track types

Creating track types is simply a matter of extending two multi-functions, `initial-pass` and `final-pass`.  `initial-pass` takes a track sequence (a sequence containing frames of the format [val len mp] where val is the value, len is the beat length of that value, and mp is the type map assigned to that track) and a type keyword, and returns a new track sequence.  Tracks might be infinite, so it's best to use a lazy function like `map` to accomplish this.  The initial pass is expected to run just before a track is returned from deftrack.  `final-pass` takes a single frame (of the same format as above) and a type keyword, and returns a new frame.  This pass runs just before a track is used as a wave.
