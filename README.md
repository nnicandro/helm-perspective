Normally `helm-buffers-list` only shows the buffers in the current perspective
since `helm` uses `ido` to construct the buffer list and `perspective`
restricts `ido`'s buffer list to the current perspective. This package supplies
the command `helm-persp-buffers` which is similar to `helm-buffers-list` but
lists the buffers in all perspectives, the buffers for each perspective being
shown in a separate `helm` source.

In addition to the default `helm-buffers-list` actions, there are actions that
are specific to working with perspectives. For example, you can set buffers
from other perspectives to the current perspective or remove buffers from their
perspective. Also, all of the `helm-buffers-list` actions are augmented in
`helm-persp-buffers` so that they operate only on the perspective of the
current `helm` source or swicth to the source's perspective whenever that seems
logical to do.

Note this package is intended to work with `perspective`
(<https://github.com/nex3/perspective-el>).
