# collector-dotnet-nuget

[![dependencies.io](https://img.shields.io/badge/dependencies.io-collector-3DA4E9.svg)](https://www.dependencies.io/docs/collectors/)
[![Docker](https://img.shields.io/badge/dockerhub-collector--dotnet--nuget-22B8EB.svg)](https://hub.docker.com/r/dependencies/collector-dotnet-nuget/)
[![GitHub release](https://img.shields.io/github/release/dependencies-io/collector-dotnet-nuget.svg)](https://github.com/dependencies-io/collector-dotnet-nuget/releases)
[![Build Status](https://travis-ci.org/dependencies-io/collector-dotnet-nuget.svg?branch=master)](https://travis-ci.org/dependencies-io/collector-dotnet-nuget)
[![license](https://img.shields.io/github/license/dependencies-io/collector-dotnet-nuget.svg)](https://github.com/dependencies-io/collector-dotnet-nuget/blob/master/LICENSE)

A [dependencies.io](https://www.dependencies.io)
[collector](https://www.dependencies.io/docs/collectors/)
that finds .NET dependencies using NuGet.

## Usage

### dependencies.yml

```yaml
collectors:
- type: dotnet-nuget
  path: /
  settings:  # all settings are optional
    example_list:
    - a
    - b
    example_string: testing
  actors:
  - ...
```

### Works well with

## Resources

- https://docs.microsoft.com/en-us/nuget/tools/nuget-exe-cli-reference
- https://hub.docker.com/r/microsoft/dotnet/

## Support

Any questions or issues with this specific collector should be discussed in [GitHub
issues](https://github.com/dependencies-io/collector-dotnet-nuget/issues). If there is
private information which needs to be shared then you can instead use the
[dependencies.io support](https://app.dependencies.io/support).
