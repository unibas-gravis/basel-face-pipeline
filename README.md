## Basel Face Registration Pipeline

This repository contains all the code to reproduce our results from our recent publication:
- Thomas Gerig, Andreas Morel-Forster, Clemens Blumer, Bernhard Egger, Marcel Lüthi, Sandro Schönborn and Thomas Vetter 
["Morphable Face Models - An Open Framework"](https://arxiv.org/abs/1709.08398)

IN: arXiv preprint (2017)

### Overview

### 0. Preparation

#### i) Folder structure and Basel reference mesh

For the registration pipeline and the experiments to work properly, some data, such as reference templates and landmarks are needed. The files are available
for download at [Registration Pipeline Data](http://gravis.dmi.unibas.ch/PMM/). The download contains the following in detail:

* Manually clicked landmarks for the BU3D-FE database.
* BFM reference mesh and expression means.
* Landmarks of the reference mesh.
* Region mask for model-building.

You can copy the content of the zip folder into `pipeline-data`. The coarse structure looks the following:

```
pipeline-data
├── data
│   ├── incoming
│   ├── bu3dfe
│   │   ├── original
├── recognition-experiment
```

If needed, you can change the location of the `pipeline-data` directory in the BU3DDataProvider.scala file.

#### iii) Bu3DFE Database

To register the BU-3DFE you have to acquire the dataset here:

[BU-3DFE](http://www.cs.binghamton.edu/~lijun/Research/3DFE/3DFE_Analysis.html)

and copy the `/original` folder to `data/bu3dfe/original/`.

### Sbt (Scala build tool)

We assume that you have sbt already installed. If not, please follow the instructions given
[here](http://www.scala-sbt.org/release/tutorial/Setup.html).

Generally you can run the code using SBT. An example is how to run it in the terminal with:

```
cd /code-directory/
sbt run
```

If you do not have enough memory use:
```
sbt -J-Xmx50g run
```

Then the different steps are then listed and can be executed by entering the number of the script or by using:
```
sbt "run-main package.Classname"
```


## Running the Pipeline

### Step 0: Data pre-processing & Folder Structure Creation

During the pipeline we do not use the BU3DFE database data directly but first convert the data to match our formats.
This step is done only once as a pre-processing and the output can be reused whenever you run a new registration.

To convert the original data from the BU3DFE database to our format use the command:

```
sbt "run-main preprocessing.ConvertBu3DRawData"
```

Explain raw data preprocessing steps in script. (The script might need some cleanup.)

### Step 1: Building the Neutral Prior Model

Pre-computing the neutral prior model can take quite some time.
However, it has to be computed only once offline and is stored in `pipeline-data/data/incoming/reference/gpmodels/`.

You can run the building process with:

```
sbt "run-main registration.BuildNeutralPrior"
```

### Step 2: Building the Core Expression Model

The core expression model augments the neutral model with expression deformations.

```
sbt "run-main registration.BuildCoreExpressionModel"
```

### Step 3: Preprocess Landmarks

This step is used to transform the reference landmarks to the new mean of the generated models and to change the uncertainty
of the individual landmarks.

```
sbt "run-main preprocessing.PrepareReferenceLandmarks"
```

### Step 4: Registration

```
sbt -J-Xmx40g "run-main registration.Registration"
```

### Step 5: Building the Morphable Model

The model building contains two steps:

 - First for each registration result the color is extracted using the input mesh.
 - Based on all meshes with color a model containing shape, color and expression variations is built.

This process may need some time and memory. Once the first step, the color extraction is computed it
can be reused if you change for example the mask of the model that should be built. But to change this
you have to out comment the corresponding line in the source code.

```
sbt -mem 40000 "run-main modelbuilding.ModelBuilding"
```
## Face Reconstruction from 2D Image

First you have to download the Multi-PIE database and copy the necessary files to the correct folders. 
This is described in the README file in the folder recogniton-experiment. 
For those experiments you need the Basel Face Model 2009 and 2017, which can be downloaded at:
[Download Basel Face Model](http://gravis.dmi.unibas.ch/PMM/)

To run the 3D reconstructions from the Multi-PIE database, you may want to execute it multiple times in parallel
since a single fit taks ~20 minutes:
```
sbt -mem 5000 "fitting.experiments.RecognitionMultiPiePose
```
And to calculate the recognition scores execute:
```
sbt -mem 5000 "fitting.experiments.RecognitionEvaluation
```
Those where the neutral scores. To perform the expression experiments, run:
```
sbt -mem 5000 "fitting.experiments.RecognitionMultiPieExpression
sbt -mem 5000 "fitting.experiments.RecognitionEvaluationEx
```



