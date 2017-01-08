(*let () = Pa_macro.linkme
let () = Pa_ounit.linkme
let () = Pa_bench.linkme
let () = Pa_type_conv.linkme
let () = Pa_js.linkme*)
let () = Pa_hardcaml.linkme

let () = Camlp4_to_ppx.main ()
