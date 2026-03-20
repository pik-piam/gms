# Package index

## Module Utilities

Inspect, create, and maintain modular GAMS model structure

- [`getModules()`](getModules.md) : getModules
- [`is.modularGAMS()`](is.modularGAMS.md) : is.modularGAMS
- [`module.skeleton()`](module.skeleton.md) : Create a Module skeleton
- [`singleGAMSfile()`](singleGAMSfile.md) : Merge GAMS code into single
  file
- [`convert.modularGAMS()`](convert.modularGAMS.md) :
  convert.modularGAMS
- [`update_modules_embedding()`](update_modules_embedding.md) : Update
  Modules Embedding in GAMS code
- [`updateInterfaceMapping()`](updateInterfaceMapping.md) :
  updateInterfaceMapping
- [`writeSets()`](writeSets.md) : writeSets

## Code Checks

Validate and check the consistency of modular GAMS model code

- [`codeCheck()`](codeCheck.md) : codeCheck
- [`checkAppearance()`](checkAppearance.md) : checkAppearance
- [`checkDescription()`](checkDescription.md) : checkDescription
- [`checkNoTabs()`](checkNoTabs.md) : checkNoTabs
- [`checkSwitchAppearance()`](checkSwitchAppearance.md) :
  checkSwitchAppearance
- [`settingsCheck()`](settingsCheck.md) : settingsCheck

## Code Parsing & Extraction

Parse and extract information from GAMS source code

- [`GAMScodeFilter()`](GAMScodeFilter.md) : GAMScodeFilter
- [`codeExtract()`](codeExtract.md) : codeExtract
- [`readDeclarations()`](readDeclarations.md) : readDeclarations
- [`readParameterValues()`](readParameterValues.md) :
  readParameterValues
- [`readSetglobals()`](readSetglobals.md) : readSetglobals
- [`readSettings()`](readSettings.md) : readSettings
- [`read_yaml_header()`](read_yaml_header.md) : read_yaml_header

## Configuration

Read, write, and validate model configuration

- [`loadConfig()`](loadConfig.md) : Load Config
- [`saveConfig()`](saveConfig.md) : Save Config
- [`readDefaultConfig()`](readDefaultConfig.md) : readDefaultConfig
- [`check_config()`](check_config.md) : Check config
- [`setScenario()`](setScenario.md) : setScenario

## Data Management

Download, distribute, and manage model input data

- [`download_distribute()`](download_distribute.md) : Download and
  unpack compressed data from repositories
- [`download_unpack()`](download_unpack.md) : Download and unpack
  compressed data from repositories
- [`publish_data()`](publish_data.md) : Publish data in a repository
- [`copy_input()`](copy_input.md) : copy_input
- [`delete_olddata()`](delete_olddata.md) : delete_olddata
- [`getfiledestinations()`](getfiledestinations.md) :
  getfiledestinations
- [`tardir()`](tardir.md) : Create tgz archive from directory
- [`fulldataOutput()`](fulldataOutput.md) : fulldataOutput
- [`update_fulldataOutput()`](update_fulldataOutput.md) :
  update_fulldataOutput

## Visualization

Visualize module interfaces and model structure

- [`interfaceplot()`](interfaceplot.md) : interfaceplot
- [`modules_interfaceplot()`](modules_interfaceplot.md) :
  modules_interfaceplot

## Model Locking

Manage concurrent access to a model folder

- [`model_lock()`](model_lock.md) [`model_unlock()`](model_lock.md)
  [`is_model_locked()`](model_lock.md) : Model lock/unlock

## Utilities

General-purpose helper functions

- [`chooseFromList()`](chooseFromList.md) : chooseFromList
- [`selectScript()`](selectScript.md) : selectScript
- [`getLine()`](getLine.md) : getLine
- [`get_info()`](get_info.md) : get_info
- [`path()`](path.md) : path
- [`readFileOrVector()`](readFileOrVector.md) : readFileOrVector
- [`replace_in_file()`](replace_in_file.md) : Replace in File
- [`model_dependencies()`](model_dependencies.md) : Function to detect R
  package dependencies
- [`gms`](gms-package.md) [`gms-package`](gms-package.md) : gms: 'GAMS'
  Modularization Support Package
