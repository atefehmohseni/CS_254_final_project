# Synthesis component

This is our prototype synthesizer, implemented in Racket using the Rosette EDSL
(version 4.0). You need to first install Rosette with

```
raco pkg install rosette
```

To run the synthesizer, run

```
racket cache_synth.rkt
```

This may take a while, depending on the configuration. The results will be
printed to stdout once synthesis is complete. See the `output` directory for
examples of what the synthesizer will output.

The synthesis parameters will depend on the ones defined in `config.rkt`. The
test cases used in our experiments are stored in `data.rkt`.
