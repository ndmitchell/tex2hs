<h1>Tex2Hs</h1>

<p>
    This tool takes a Latex document, usually intended for consumption with <a href="http://www.informatik.uni-bonn.de/~loeh/lhs2tex/">lhs2tex</a>, and attempts to type check it. While <a href="http://www.haskell.org/onlinereport/literate.html">literate Haskell</a> is great for writing a document which is a complete piece of code, when writing fragments of code interspersed with alternative definitions it can be tricky. Tex2hs takes a .tex file, and automatically tries to splice the fragments together in a way that type checks.
</p>

<h2>An Example</h2>

<p>
    Let's imagine we are writing a paper:
</p>
<pre>
We have developed a method for short-cut fusion using |foldr|/|build|.
By introducing the definition:

\begin{code}
build :: ((a -> [a] -> [a]) -> [b] -> c) -> c
build g = g (:) []
\end{code}

The standard definition of |map| is:

\begin{code}
map :: (a -> b) -> [a] -> [b]
map f []      = []
map f (x:xs)  = f x : map f xs
\end{code}

But we redefine it as:

\begin{code}
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []
\end{code}

Which now allows us to have the property:

\begin{code}
propEq g k z = foldr k z (build g) == g k z
\end{code}
</pre>
<p>
    This code won't actually type check in a literate document -- for a start <tt>map</tt> has been defined once in the Prelude, and twice in the paper. We also need to link the <tt>build</tt> into the <tt>propEq</tt>. If we try running Tex2hs we get:
</p>
<pre>
$ tex2hs sample.tex
Checking line 1... success
Checking line 1... success
Checking line 3... success
Checking line 8... success
Checking line 10... FAILURE
  ERROR "temp.hs":4 - Ambiguous variable occurrence "map"
  *** Could refer to: Main.map Hugs.Prelude.map
</pre>
<p>
    We can fix this error easily by creating a file <tt>Include.hs</tt> in the same directory as the paper with the text <tt>import Prelude hiding (map)</tt>. This file is a great place to put definitions and imports which are necessary. Running the checker again, this time specifying to go from line 10:
</p>
<pre>
$ tex2hs sample.tex 10..
Checking line 10... success
Checking line 18... FAILURE
  ERROR "temp.hs":5 - Type error in explicitly typed binding
  *** Term           : map
  *** Type           : (([a] -> [a]) -> [b] -> [b]) -> [a] -> [b]
  *** Does not match : (([a] -> [a]) -> [b] -> [b]) -> [[a] -> [a]] -> [[b] -> [b]]
  *** Because        : unification would give infinite type
</pre>
<p>
    We can now spot the bug, it should have been <tt>(:) . f</tt>, not the other way round. We fix this bug, and now the entire document type checks.
</p>
