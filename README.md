# DataCleanup
Take MZmine output and perform blank removal, transient feature removal, and normalizing

The RAW folder needs to contain:

orbitrapsequence.csv
_Has 3 columns with the names: "File Name", "Sample Name", "Injection_Type"_

metadata.csv
_Has at least the columns: "Sample Name" (matching orbitrap sequence)_

POSTgapfilled.csv
_Output from MZmine, after gapfilling or after the last step._

PREgapfilled.csv
_Output from MZmine before the gapfilling step_
