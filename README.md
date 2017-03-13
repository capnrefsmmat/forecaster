Forecaster
----------

Generates plain-text weather forecasts, optionally formatted as emails.

I was fed up with commercial weather apps and websites: the websites tend to be
monstrosities with several megabytes of custom fonts and images which take
twenty seconds to finish loading, and the forecasts lean towards alarmism, often
forecasting five inches of snow ten days in advance when there's simply no way
to forecast that accurately.

The UI of apps and websites usually sucks too. Each morning I just want to know
(a) how warm or cold it will be throughout the day, (b) whether there'll be rain
or snow, and (c) if there are any alerts or big events coming. Fancy charts and
graphs are usually pointless; I don't need to see trends ten days into the
future because I only use the information to decide whether to wear a coat in
the morning.

`forecaster` uses the new National Weather Service API to answer these
questions, generating a simple plain-text summary of the next few days. I've
hooked this up to a shell script to deliver mail to my inbox every morning (via
[notmuch mail](https://notmuchmail.org/)), but you could hook it up any way you
want -- a command-line summary, email, RSS feed, or whatever. Presumably you
could even hook it up to a thermal printer and a Raspberry Pi to have a forecast
printed every morning.

An example forecast email is shown in `example-forecast.txt`.

License (MIT)
-------------

Copyright 2017 Alex Reinhart

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
