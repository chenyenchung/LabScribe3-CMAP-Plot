# LabScribe 3 Exported Data Visualizer
It's an amateur practice, originally with lots of need-to-be-fixed loops. Thanks to the use of *dplyr* and the instructions about `apply` functions. Hopefully it works better now.

- Purpose: It draws an overview waveform graph and a summary boxplot of the wave amplitude. Both graphic is faceted by assay time point and sample type
- Pipeline: The script would first sort the raw data and keep the top M waves that are balanced, and then among them pick the top N waves with biggest amplitude (This is what the script *data filter* does). After filtering, the waves would drawn for each timepoint, stimulation strength, and sampletype (This is what the script *graph maker* does).
- What you need to set (in *settings.R*):
  - Assay date: This would add an assaydate to the filtered data, and the script would count sample age upon experiment according to this, and **you would need to annotate sample date of birth in the file name quoted by a pair of parentheses ()**
  - Sample type: The data filtering script would try to find the words provided here and annotate this as the sample type (*e.g., "Wild type", "Mutant"*)
  - Treatment type: The script would then try to find these words provided here and annotate this as the treatment type (*e.g., "drug X", "Sham"*)
  - Frame number: As in variable `frameNum`. It defines how many data points to draw (1 point = 1 frame = 2 ms)
  - Frame before stimulation: As in variable `prelude`. It defines how many data points to draw before the stimulation.
  - Pick balanced peak: Pick the top M waves that are balanced. (Variable `pickBal`)
  - Pick biggest peak: Pick the top N waves with biggest amplitude. (Variable `pickPeak`)
  - Save folder: The folder to save filtered data table that would be read by the *graph maker* script

## At the moment it can...
### 2017/10/13

* Draw a exploratory plot for CMAP, faceted by sample type, assay timepoint, treatment, stimulation strength. The plot would contain wave plots and boxplots for wave amplitudes
* The re-write process seems to be finished (~~hopefully~~)

### 2017/6/3

* Just draw a few milliseconds (you decide) after every single stimulation
* Draw the mean signal per experiments (one experiment defined as stimulation with one particular voltage), and then save as a pdf graph in your working directory
  * Calculat the average and standard deviation of amplitudes and save it in an individual .txt table

### 2017/6/27
* Try to rewrite the script to reduce excessive for-loops, or it is just too messy to maintain... (under construction)
