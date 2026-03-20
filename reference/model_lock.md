# Model lock/unlock

Lock the model to coordinate multiple processes accessing the model.
This is necessary if you change files within the model and have to avoid
that another process reads from your half-written changes in the mean
time. model_lock creates an exclusive lock on the given file and returns
a lock id. When the lock id is handed to model_unlock the lock is
dropped again, and another process can take the lock. The lock is also
dropped when the lock id goes out of scope and is garbage collected in R
or when the R process terminates or crashes. However, it is good
practice to explicitly call model_unlock when you don't need the lock
anymore. If you want to check for informational purposes if the model is
currently locked by another process, you can use the is_model_locked()
function. Note however that you should never use is_model_locked() to
coordinate access to the model folder. The return value of
is_model_locked() is only a snapshot at the time when the function is
run. Note additionally that is_model_locked() checks if the model is
locked by another process. If you currently hold the lock of the model
yourself, it will return FALSE.

## Usage

``` r
model_lock(folder = ".", file = ".lock", timeout1 = 12, timeout2 = NULL,
check_interval = NULL, oncluster = NULL)
model_unlock(id, folder = NULL, file = NULL, oncluster = NULL)
is_model_locked(folder = ".", file = ".lock")
```

## Arguments

- folder:

  path to the model folder

- file:

  file name of the lock file. Don't use the lock file for anything else
  (i.e. don't read or write or open the lock file).

- timeout1:

  Time in hours to wait for other processes to free the lock before
  giving up and exiting with an error.

- timeout2:

  deprecated setting which will be ignored.

- check_interval:

  deprecated setting which will be ignored.

- oncluster:

  deprecated setting which will be ignored.

- id:

  lock id as returned by model_lock.

## Value

model_lock returns the lock id which is needed to identify the lock in
model_unlock.

## See also

[`check_config`](check_config.md)

## Author

Jan Philipp Dietrich, David Klein, Mika Pflüger

## Examples

``` r
#lock folder
id <- model_lock(tempdir())
#> 2026-03-20 10:04:23: try to acquire model lock...
#> 2026-03-20 10:04:23: acquired model lock in 0 secs.

#unlock folder
model_unlock(id)
#> 2026-03-20 10:04:23: unlocked model.
```
