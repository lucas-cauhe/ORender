## Render a scene
`dune exec render`

## Run the tonemapper
`dune exec tonemapper`

## Compare noise
`dune exec noise -- -in <noisy_img> -ref <reference_img>`

## Build docs
`dune build @doc`

## Run tests
`dune runtest`

## Input parameters of the render

- Image resolution
- Number of rays per pixel (rpp)
- K<sub>d</sup>

lib/dune
(install
 (section lib)
 (files (computer_gfx.a)))

ocamlfind ocamlcp -p a computer_gfx.cma -o main -package domainslib -package computer_gfx -package progress -linkpkg -thread  bin/main.ml
