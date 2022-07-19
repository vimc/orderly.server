# orderly.server API

<!--

To update the spec, the easiest thing to do is to:

    ./scripts/redis start
    ./scripts/demo

which will start a server on 8080 with a bunch of reports in it already. A table of committed reports will be printed.

-->

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

Each task has a `status` which can be:
* queued - report is waiting in queue for an available worker
* running - report is being run on a worker
* success - report has completed successfully
* error - report has completed with an error
* orphan - report killed by external process, has crashed or worker has died
* interrupted - report running was cancelled, either via timeout or `/reports/:key/kill/` endpoint
* deferred - report is waiting on a dependency to be available before being put into queue
* impossible - report has a dependency which will never be met i.e. its dependency errored, was cancelled or the worker died
* missing - report is not known

Note these are equivalent to status from [rrq task lifecycle](https://mrc-ide.github.io/rrq/reference/rrq_controller.html) but with some small name changes for backward compatibility.

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
            "version": "20210211-143212-98c45632",
            "inputs": {
                "name": "minimal",
                "params": null,
                "ref": null,
                "instance": null,
                "changelog": null
            }
        },
        {
            "key": "flavoured_bassethound",
            "status": "queued",
            "version": null,
            "inputs": {
                "name": "other",
                "params": { "nmin": "0.5" },
                "ref": null,
                "instance": null,
                "changelog": "[internal] changelog message"
            }
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
        "version": "20210211-143212-98c45632",
        "inputs": {
            "name": "minimal",
            "params": null,
            "ref": "3e0d1d2",
            "instance": "production",
            "changelog": null
        }
    },
    {
        "key": "flavoured_bassethound",
        "status": "queued",
        "version": null,
        "inputs": {
            "name": "other",
            "params": { "nmin": "0.5" },
            "ref": null,
            "instance": null,
            "changelog": "[internal] changelog message"
        }
    }
]
```

## DELETE /reports/:key/kill/

Kill a running report. Returns boolean `killed`, `true` if successfully killed otherwise `false`. If killing fails will return the reason in `message` property.

Schema: [`Kill.schema.json`](Kill.schema.json)

### Example

``` json
{
    "killed": true,
    "message": null
}
```

``` json
{
    "killed": false,
    "message": "Failed to kill 'credulous_electriceel'\n  Task 0ca2a05ae01d0f2afea5c166a28983b6 is not cancelable (CANCELLED)"
}
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
* With a branch other than master (& optionally a commit) and param `show_all=true` to list all reports available on a branch for a certain commit. If commit is empty then this will use the most recent commit
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

## GET /reports/info

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

## POST /workflow/summary

Takes an array of reports to be run and returns info about how they will be run. Including the order they will be submitted to the queue and details of any missing immediate dependencies.

Request schema: [`WorkflowSummaryRequest.schema.json`](WorkflowSummaryRequest.schema.json)

Response schema: [`WorkflowSummaryResponse.schema.json`](WorkflowSummaryResponse.schema.json)

### Example

Request
```json
{
  "reports": [
    {
      "name": "process",
      "instance": "production"
      "params": {
        "nmin": 0.5,
        "nmax": 2
      }
    },
    {
      "name": "postprocess",
      "instance": "production"
    }
  ],
  "ref": "cf57b021cf4d8597e9c703a54582090005bef830"
}
```

Response
```json
{
  "reports": [
    {
      "name": "process",
      "instance": "production",
      "params": {
        "nmin": 0.5,
        "nmax": 2
      }
      "depends_on": ["preprocess"]
    },
    {
      "name": "postprocess",
      "instance": "production",
      "depends_on": ["preprocess", "process"]
    }
  ],
  "ref": "cf57b021cf4d8597e9c703a54582090005bef830",
  "missing_dependencies": {
    "process": ["preprocess"],
    "postprocess": ["preprocess"]
  }
} 
```

## POST /workflow/run

Run a set of reports as a workflow. Takes a set of reports to run, and any instance, params or dependencies between the reports. Works out the order the reports can be sent to the queue respecting any dependencies and adds them to the queue. Returns the ID of the workflow and each individual report. If 1 report fails reports dependent on the failed report will have status set to "impossible". Any independent reports will still be run.

Request schema: [`WorkflowRunRequest.schema.json`](WorkflowRunRequest.schema.json)

Response schema: [`WorkflowRunResponse.schema.json`](WorkflowRunResponse.schema.json)

### Example

Request
```json
{
  "reports": [
    {
      "name": "other",
      "params": {
        "nmin": [0.5]
      }
    },
    {
      "name": "minimal"
    }
  ],
  "ref": "695d4e1673919ae77babc64b39ba26e9c844efa5",
  "changelog": {
    "message": "changelog 1",
    "type": "internal"
  }
} 
```

Response

```json
{
  "status": "success",
  "errors": null,
  "data": {
    "workflow_key": "aghast_wolf",
    "reports": [
      {
        "name": "other",
        "params": {
          "nmin": 0.5
        },
        "key": "deific_thrip",
        "execution_order": 1
      },
      {
        "name": "minimal",
        "key": "monarchistic_blackmamba",
        "execution_order": 2
      }
    ]
  }
}
```

## GET /workflow/status

Get the status of a running workflow. Finds list of reports within this workflow and gets the status of each of the reports. Note the format of the status of each report matches the format of the `/reports/:key/status/` endpoint. The status of the workflow is a combination of the status of the individual reports. Values it can take are:
* success - if all reports in workflow have completed successfully
* running - if at least one of the reports is running
* cancelled - if at least one of the reports has been cancelled
* queued - if all the reports are either queued or deferred
* error - otherwise

Response schema: [`WorkflowStatus.schema.json`](WorkflowStatus.schema.json)

### Example


```json
{
  "status": "success",
  "errors": null,
  "data": {
    "workflow_key": "prophetic_ankolewatusi",
    "status": "success",
    "reports": [
      {
        "key": "chartreuse_alleycat",
        "status": "success",
        "version": "20220211-171617-6b2b2cac",
        "output": [
          "[ git        ]  fetch",
          "[ checkout   ]  579dd44fa71f579cbe5669578718c957015a71f3; was master",
          "[ name       ]  minimal",
          "[ id         ]  20220211-171617-6b2b2cac",
          "[ id_file    ]  /tmp/RtmpUyRoEL/file30698f4fe64e83/runner/id/chartreuse_alleycat.id_file",
          "[ start      ]  2022-02-11 17:16:17",
          "[ git        ]  checkout master; was HEAD",
          "[ data       ]  source => dat: 20 x 2",
          "",
          "> png(\\"mygraph.png\\")",
          "",
          "> par(mar = c(15, 4, 0.5, 0.5))",
          "",
          "> barplot(setNames(dat$number, dat$name), las = 2)",
          "",
          "> dev.off()",
          "null device ",
          "          1 ",
          "[ end        ]  2022-02-11 17:16:17",
          "[ elapsed    ]  Ran report in 0.34585 secs",
          "[ artefact   ]  mygraph.png: 4a9b9329e8de721c9a081cb8f982c21f",
          "[ commit     ]  minimal/20220211-171617-6b2b2cac",
          "[ copy       ]",
          "[ import     ]  minimal:20220211-171617-6b2b2cac",
          "[ success    ]  :)",
          "id:20220211-171617-6b2b2cac"
        ],
        "queue":[]
      },
      {
        "key": "panphobic_seabird",
        "status": "success",
        "version": "20220211-171616-61ccb4c1",
        "output": [
          "[ git        ]  fetch",
          "[ checkout   ]  579dd44fa71f579cbe5669578718c957015a71f3; was master",
          "[ name       ]  global",
          "[ id         ]  20220211-171616-61ccb4c1",
          "[ id_file    ]  /tmp/RtmpUyRoEL/file30698f4fe64e83/runner/id/panphobic_seabird.id_file",
          "[ global     ]  data.csv -> data.csv",
          "[ start      ]  2022-02-11 17:16:16",
          "[ git        ]  checkout master; was HEAD",
          "",
          "> data <- read.csv(\\"data.csv\\")",
          "",
          "> saveRDS(data, \\"out.rds\\")",
          "[ end        ]  2022-02-11 17:16:16",
          "[ elapsed    ]  Ran report in 0.01059127 secs",
          "[ artefact   ]  out.rds: 94bce2af59aeeba7e0d556344ed45285",
          "[ commit     ]  global/20220211-171616-61ccb4c1",
          "[ copy       ]",
          "[ import     ]  global:20220211-171616-61ccb4c1",
          "[ success    ]  :)",
          "id:20220211-171616-61ccb4c1"
        ],
        "queue":[]
      }
    ]
  }
}
```

## GET /reports/

Return a list of all reports with minimal metadata - the id, human readable name and latest version of each.
Optionally pass a query string `reports` with a list of report names to filter to.

### Example

`GET /reports/?reports=minimal,use_resource`

```json

  [
    {"name": "minimal", "display_name": "Minimal example", "latest_version": "20161010-121958-d5f0ea63"},
    {"name": "use_resource", "display_name": "Use resources example", "latest_version": "20171011-121958-effh734"}       
  ]

```

## GET /reports/:name

Returns a list of version names for the named report. Returns 404 if no versions exist.

Schema: [`VersionIds.schema.json`](VersionIds.schema.json)

## Example

```json
[
    "20161006-142357-e80edf58",
    "20161008-123121-59891d61",
    "20161012-220715-756d55c8"
]
```

## GET /reports/:name/versions/:version/

Returns metadata about a single report version, including custom fields

Schema: [`ReportVersion.schema.json`](VersionDetails.schema.json)

### Example

```json
{
  "id": "20161006-142357-e80edf58",
  "name": "minimal",
  "displayname": null,
  "description": null,
  "artefacts": [
    {
      "format": "staticgraph",
      "description": "A graph of things",
      "files": [
        "mygraph.png"
      ]
    }
  ],
  "resources": [
    {
      "name": "source/inputdata.csv",
      "size": 20
    }
  ],
  "date": "2016-10-06 14:23:57.0",
  "data_info": {
    "name": "extract",
    "csvSize": 751,
    "rdsSize": 559
  },
  "parameter_values": {
    "param1": "paramValue1",
    "param2": "paramValue2"
  },
  "instances": {
    "source": "science"
  },
  "requester": "Funder McFunderface",
  "author": "Researcher McResearcherface"
}
```

## GET /reports/:name/versions/:id/artefacts

Get a dictionary of artefact names to hashes.
Returns a 404 if the provided report name-version combination does not exist.

Schema: [`Artefacts.schema.json`](Artefacts.schema.json)

## Example

```json
{
  "status": "success",
  "errors": null,
  "data": {
    "mygraph.png": "7360cb2eed3327ff8a677b3598ed7343"
  }
}
```

# GET /reports/versions/customFields?versions=

Get custom fields for a list of version ids.

Response schema: [`CustomFieldsForVersions.schema.json`](CustomFieldsForVersions.schema.json)

# Example

`GET /reports/versions/customFields?versions=20210629-231827-d35633fd,20210730-152428-14ad0fe7`

```json
{
  "status": "success",
  "errors": null,
  "data": {
    "20210629-231827-d35633fd": {
      "requester": "Funder McFunderface",
      "author": "Researcher McResearcherface",
      "comment": "This is a comment"
    },
    "20210730-152428-14ad0fe7": {
      "requester": "Funder McFunderface",
      "author": "Researcher McResearcherface",
      "comment": "This is a comment"
    }
  }
}
```

If non-existent ids are given the response is

```json
{
  "status": "success",
  "errors": null,
  "data": {}
}
```

# GET /reports/customFields

Get custom fields for orderly instance

Response schema: [`CustomFields.schema.json`](CustomFields.schema.json)

# Example

```json
["author", "requester"]
```

# GET /reports/versions/parameters?versions=

Get parameters for a list of version ids.

Response schema: [`Parameters.schema.json`](Parameters.schema.json)

# Example

`GET /reports/versions/parameters?versions=20210629-231827-d35633fd,20210730-152428-14ad0fe7`

```json
{
  "status": "success",
  "errors": null,
  "data": {
    "20210629-231827-d35633fd": {
      "nmin": "0.1"
    },
    "20210730-152428-14ad0fe7": {
      "disease": "YF"
    }
  }
}
```

If non-existent ids are given the response is

```json
{
  "status": "success",
  "errors": null,
  "data": {}
}
```
