'use strict';

import * as vscode from "vscode"

import { Commands } from "./commands"

export class DottyDebugConfigurationProvider implements vscode.DebugConfigurationProvider {
  constructor(private outputChannel: vscode.OutputChannel) {
  }
  
  // Returns an initial debug configurations based on contextual information.
  public provideDebugConfigurations(folder: vscode.WorkspaceFolder | undefined, token?: vscode.CancellationToken):
  vscode.ProviderResult<vscode.DebugConfiguration[]> {
    return [{
      type: "dotty",
      name: "Debug (Launch)",
      request: "launch",
      mainClass: "",
      args: "",
    }, {
      type: "dotty",
      name: "Debug (Attach)",
      request: "attach",
      hostName: "localhost",
      port: 0,
    }]
  }

  // Try to add all missing attributes to the debug configuration being launched.
  public resolveDebugConfiguration(folder: vscode.WorkspaceFolder | undefined, config: vscode.DebugConfiguration,
      token?: vscode.CancellationToken): vscode.ProviderResult<vscode.DebugConfiguration> {
    return this.heuristicallyResolveDebugConfiguration(folder, config)
  }

  private async heuristicallyResolveDebugConfiguration(folder: vscode.WorkspaceFolder | undefined,
      config: vscode.DebugConfiguration) {
    try {
      this.outputChannel.append(`Resolving ${config.name}`)

      if (config.request == "launch") {
        if (!config.mainClass) {
          vscode.window.showErrorMessage("Please specify the mainClass in the launch.json.")
          return undefined
        } else if (!config.classPaths || !Array.isArray(config.classPaths) || !config.classPaths.length) {
          config.classPaths = await resolveClasspath(config.mainClass, config.projectName)
        }
        if (!config.classPaths || !Array.isArray(config.classPaths) || !config.classPaths.length) {
          vscode.window.showErrorMessage("Cannot resolve the classpaths automatically, please specify the value in the launch.json.")
          return undefined
        }
      }

      const debugServerPort = await startDebugSession()
      if (debugServerPort) {
        this.outputChannel.append(`DDS started on port: ${debugServerPort}`)
        config.debugServer = debugServerPort
        return config
      } else {
        this.outputChannel.append("DDS failed to start")
        return undefined
      }
    } catch (ex) {
      const errorMessage = (ex && ex.message) || ex
      vscode.window.showErrorMessage(String(errorMessage))
    }
  }
}

export function executeDottyLanguageServerCommand(...rest) {
    return vscode.commands.executeCommand(Commands.EXECUTE_WORKSPACE_COMMAND, ...rest)
}

function startDebugSession() {
    return executeDottyLanguageServerCommand(Commands.START_DEBUG_SESSION)
}

function resolveClasspath(mainClass, projectName) {
  // return executeDottyLanguageServerCommand(Commands.RESOLVE_CLASSPATH, mainClass, projectName)
  return [
    "/home/smarter/opt/dotty/interfaces/target/classes",
    "/home/smarter/opt/dotty/library/../out/bootstrap/dotty-library-bootstrapped/scala-0.4/classes",
    "/usr/lib/jvm/java-8-openjdk-amd64/lib/tools.jar",
    "/home/smarter/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.3.jar",
    "/home/smarter/.ivy2/cache/org.scala-lang.modules/scala-asm/bundles/scala-asm-5.2.0-scala-2.jar",
    "/home/smarter/.ivy2/cache/com.typesafe.sbt/sbt-interface/jars/sbt-interface-0.13.15.jar",
    "/home/smarter/.ivy2/cache/org.scala-lang.modules/scala-xml_2.12/bundles/scala-xml_2.12-1.0.6.jar",
    "/home/smarter/opt/dotty/compiler/../out/bootstrap/dotty-compiler-bootstrapped/scala-0.4/classes"
  ]
}

// function resolveClasspath(mainClass, projectName) {
//     return executeJavaLanguageServerCommand(commands.JAVA_RESOLVE_CLASSPATH, mainClass, projectName);
// }