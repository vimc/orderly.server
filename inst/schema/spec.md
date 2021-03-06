# orderly.server API

This API is built on top of [`porcelain`](https://reside-ic.github.io/porcelain) (itself influenced by [`hintr`](https://github.com/mrc-ide/hintr) and [montagu api](https://github.com/vimc/montagu-api/blob/master/spec/spec.md)).


## GET /

Return package name and version

## POST /reports/:name/run/

Try and run a report `:name`

Accepts as `POST` body json containing `params` and `changelog` that will be passed through to the report.  This is required when the report requires parameters and is not allowed for reports that do not allow parameters. e.g.

```
{
  "params": {
    "nmin": 0.5
  },
  "changelog": {
    "type": "internal",
    "message": "Added new output plot"
  }
}
```

Accepts the query parameter `ref`, to try running the report against a particular git reference (e.g., a branch or a commit).

Accepts the query parameter `timeout`, which sets the the number of seconds to wait before the job is terminated.  The default is 3 hours (10800 seconds).

Accepts the query parameter `instance`, to run the report against a particular source database.

Returns information to query the status of the report via the next endpoint

Schema: [`Run.schema.json`](Run.schema.json)

### Example

``` json
{
    "name": "report-name",
    "key": "adjective_animal",
    "path": "/v1/reports/adjective_animal/status"
}
```

## GET /reports/:key/status/

Get the status of a report.

Accepts query parameter `output`, which if TRUE returns the log from the job run.

This works only for reports that were queued by the runner itself.

Schema: [`Status.schema.json`](Status.schema.json)

### Example queued status

```json
{
    "key": "adjective_animal",
    "status": "queued",
    "version": null,
    "output": null,
    "queue": [
        {
            "key": "antiutopian_peregrinefalcon",
            "status": "running",
            "name": "minimal"
        },
        {
            "key": "flavoured_bassethound",
            "status": "queued",
            "name": "other"
        }
    ]
}
```

### Example completed status

```json
{
    "key": "adjective_animal",
    "status": "success",
    "version": "20170912-091103-41c62920",
    "output": [
        "[ name      ]  example",
        "[ id        ]  20170912-091103-41c62920",
        "[ id_file   ]  /var/folders/3z/86tv450j7kb4w5y4wpxj6d5r0000gn/T//RtmpozkWqn/fileaf521bb78e78",
        "[ data      ]  dat: 20 x 2",
        "[ start     ]  2017-09-12 09:11:03",
        "",
        "> png(\"mygraph.png\")",
        "",
        "> par(mar = c(15, 4, 0.5, 0.5))",
        "",
        "> barplot(setNames(dat$number, dat$name), las = 2)",
        "",
        "> dev.off()",
        "null device ",
        "          1 "
        "[ end       ]  2017-09-12 09:11:03",
        "[ artefact  ]  mygraph.png: b7de1d29f37d7913392832db6bc49c99",
        "[ commit    ]  example/20170912-091103-41c62920",
        "[ copy      ]",
        "[ success   ]  :)",
        "id:20170912-091103-41c62920"
    ],
    "queue": []
}
```

## GET /queue/status/

Get the status of the queue.

This returns the key, report name and status of jobs. It includes jobs which are actively running and jobs which are waiting in the queue.

Schema: [`QueueStatus.schema.json`](QueueStatus.schema.json)

### Example queued status

```json
[
    {
        "key": "antiutopian_peregrinefalcon",
        "status": "running",
        "name": "minimal"
    },
    {
        "key": "flavoured_bassethound",
        "status": "queued",
        "name": "other"
    }
]
```

## DELETE /reports/:key/kill/

Kill a running report.  If the report was running then `true` will be returned.  Otherwise an error will be thrown.

Schema: [`Kill.schema.json`](Kill.schema.json)

### Example

``` json
true
```

## GET /reports/git/status/

Get git status.  This does not quite map onto `git status` but includes output from `git status --porcelain=v1` along with branch and hash information.  When running on a server, ideally the `output` section will be an empty array (otherwise branch changing is disabled)

## Example

```json
{
  "branch": "master",
  "hash": "1ed7a67351b03cddbb27d5cb8db184fbd8b2ab0c",
  "clean": true,
  "output": []
}
```

## POST /reports/git/fetch/

Fetch from remote git.  This is required before accessing an updated reference (e.g. a remote branch) or a hash not present in the local git tree.  It's always safe because it does not change the working tree

## Example

```json
[
  "From /tmp/RtmpT2bd1r/file138f7147a05/demo",
  "   ba72f7a..ed3d168  master     -> origin/master"
]
```


## POST /reports/git/pull/

Pull from remote git.  This updates the working tree

## Example

```json
[
  "Updating ba72f7a..ed3d168",
  "Fast-forward",
  " new | 1 +",
  " 1 file changed, 1 insertion(+)",
  " create mode 100644 new"
]
```


## GET /git/branches/

List git branches whose tips are not reachable from master branch i.e. branches with commits which haven't been merged into master and master branch itself.

## Example

```json
[
  {
    "name":"master",
    "last_commit":"2021-01-07 12:07:02",
    "last_commit_age":23
  },
  {
    "name":"other",
    "last_commit":"2021-01-07 12:07:02",
    "last_commit_age":23
  }
]
```


## GET /git/commits/

List commits hashes, age and time of commit from a branch. If master branch then lists last 25 commits. If any other branch than master it lists unmerged commits up to limit of 25. 

## Example

```json
[
  {
    "id":"b0c9c90",
    "date_time":"2021-01-07 12:07:02",
    "age":173
  }
]

```


## GET /run-metadata/

Get metadata for Orderly Web report runner interface to control UI display

## Example

```json
{
  "name": "science",
  "instances_supported": false,
  "git_supported": true,
  "instances": {
    "source":[]
  },
  "changelog_types": [
    "public"
  ]
}
```


## GET /reports/source

List reports which can be run by Orderly Web. 

Reports can be listed
* With no branch or commit - lists all reports in src dir on latest commit on master branch
* With a branch other than master & unmerged commit - lists all reports which have changes in them from this commit compared with master branch 
* With master branch and a commit - lists all reports in src dir on this commit

## Example

```json
[
  "global",
  "minimal"
]
```


## GET /reports/<report_id>/parameters

List parameters and any default values for a report on a specific commit.

## Example

```json
[
  {
    "name":"nmin",
    "default":null
  }
]
```

## GET /reports/<report_name>/dependencies

Get the dependency tree for a report. The root dependency in the returned tree is always the report specified in the request.

Accepts the query parameter `id`, to specify the report version (default is `latest`)

Accepts the query parameter `direction`, to specify dependency type, `upstream` or `downstream` (default is `"downstream"`)

Accepts the query parameter `propagate`, to indicate whether out of date results should be propagated through the tree (default is `true`)

Accepts the query parameter `max_depth` to indicate the maximun number of levels which will be returned in the tree (default is `100`)

Accepts the query parameter `show_all` to indicate whether all reports should be returned in the tree, not just the latest (default is `false`)

Accepts the query parameter `use` to specify where to find dependencies, `src` or `archive` (default is `archive`) 

## Example

```json
{
  "direction": "upstream",
  "dependency_tree": {
      "name": "r1",
      "id": "20200222-171004-c72b21",
      "out_of_date": true,
      "dependencies": [
        {
          "name": "r2",
          "id": "20210221-091112-b172f",
          "out_of_date": false,
          "dependencies": []
        },
        {
          "name": "r3",
          "id": "20210221-100336-dd312",
          "out_of_date": false,
          "dependencies": []
        }
      ]
  }
}
```

Schema: [`Dependencies.schema.json`](Dependencies.schema.json)

## POST /bundle/pack/:name

Create a "[report bundle](https://www.vaccineimpact.org/orderly/reference/orderly_bundle_pack.html)", to be run on some other machine

As for the `/reports/:name/run/` endpoint, accepts as `POST` body json that will be passed directly through to the report.  This is required when the report requires parameters and is not allowed for reports that do not allow parameters.

Also accepts the query parameter `instance` which can be used to change database instance.

Returns binary data, being the created bundle object for use with `orderly::orderly_bundle_run`

## POST /bundle/import

Import a bundle previously packed with `/bundle/pack/:name`. Requires the completed bundle to be sent as body with mime type of `application/octet-stream`.

Schema: [`BundleImport.schema.json`](BundleImport.schema.json)

### Example

``` json
true
```

## GET /report/info

Get info about a report run. This can either be from a successful or failed run in draft or archive.

Takes parameters `id` and `name` of the report you want to retrieve info for.

Response schema: [`ReportInfo.schema.json`](ReportInfo.schema.json)

### Example

```json
{
  "id": "20210303-093616-906ae61b",
  "name": "minimal",
  "success": false,
  "date": "2021-03-03 09:36:16",
  "elapsed": 0.0345,
  "git": {
    "branch": "master",
    "ref": "b7c6b0c"
  },
  "logfile": "path/to/log",
  "error": {
    "message":"some error",
    "trace": [
      "orderly_version.R#602: source(private$recipe$script)",
      "withVisible(eval(ei, envir))",
      "eval(ei, envir)",
      "eval(ei, envir)",
      "script.R#5: stop(\"some error\")"
    ]
  },
  "params": {
    "nmin": 0.1
  }
}
```
