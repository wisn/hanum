# Hanum
An OpenStreetMap attributes linter with custom presets.

## Introduction
Name: Wisnu Adi Nurcahyo

Education: Informatics Engineering '16 at Telkom University

Contact: <wisnu@nurcahyo.me>

GitHub: <https://github.com/wisn/>

## Summary
Create a dynamic linter for the OSM attributes.
This Linter will check and evaluate the attribute validity.
Everyone may contributes to create the rules with a preset.
Both the presets nor the OSM file must be in a JSON.

## Benefits
The linter will filtering any possible error.
So, it’s will reduce the possible conflict when submitting data to OpenStreetMap.
We may creates our own presets for the rules.
This mean, for each country, their rules will be available.

## Details
The linter will take OSM attributes that has been converted to JSON.
Then, evaluate it with our rules through a presets.
If the attributes didn't meet with our rules, it's will show an error message.

The presets itself looks like this. Assume this is `sample.presets.json` file.
```json
{
  "name": "Sample Presets",
  "author": "Wisnu Adi Nurcahyo",
  "locale": "ID",
  "applied": ["building"],
  "presets": {
    "address": ["required", "startedWith Jl."],
    "short_name": ["dontSameWith name", "onlyWords"]
  }
}
```

As you can see, the presets need a target `key` (attribute name).
Then, the `value` is rule function like `startedWith`.
The rule functions are predefined in the linter program.

Sample of rule functions:
- **required**: Is the attribute exist?
- **startedWith**: Is the attribute started with defined words?
- **dontSameWith**: Is the attribute same with another attribute?
- **onlyWords**: Is the attributes only contain a words?

Later, this linter will have some `default presets`. Example,

```
$ hanum sample.json
Using "Sample Presets" for "building" with locale "ID"
...
```

The "Sample Presets" have `locale` and `applied` field.
Default presets will be used if the attributes is match with the presets information header.

If tester using custom presets, it's will looks like this.

```
$ hanum sample.json sample.presets.json
Using "Sample Presets"
...
```

With this power, we may use standardized presets or our own presets.
The standardized presets is a presets that created by OpenStreetMap.

## Demo
This demo is based on the prototype.
Since the details above is the concept, some presets below will be different.
Assume we have OSM attributes in JSON like this.
Saved as `sample.json`.
```json
{
  "version": 0.6,
  "generator": "Overpass API",
  "osm3s": {
    "timestamp_osm_base": "2016-12-11T04:06:02Z",
    "copyright": "The data included in this document is from www.openstreetmap.org. The data is made available under ODbL."
  },
  "elements": [
    {
      "type": "way",
      "id": 400611312,
      "nodes": [
        4031547479,
        4031547481,
        4031547475,
        4031547474,
        4031547464,
        4031547458,
        4031547456,
        4031547452,
        4031547455,
        4031547453,
        4031547448,
        4031547445,
        4031547450,
        4031547457,
        4031547460,
        4031547469,
        4031547463,
        4031547467,
        4031547472,
        4031547477,
        4031547470,
        4031547473,
        4031547476,
        4031547483,
        4031547478,
        4031547480,
        4031547482,
        4031547488,
        4031547484,
        4031547489,
        4031547479
      ],
      "tags": {
        "admin_level": "6",
        "building": "yes",
        "name": "Kantor Kecamatan Bintang Ara",
        "office": "government",
        "short_name": "Kantor Camat Bintang Ara",
        "wikipedia": "id:Bintang_Ara,_Tabalong"
      }
    }
  ]
}
```

The presets is `sample.presets.json` below.
```json
{
  "presets": {
    "short_name": "dontSameWith name",
    "address": "required"
  }
}

```

So, we now can lint it.

```
$ hanum sample.json sample.presets.json
[ERROR] Required key: address
```

**Explanation**: Since there is didn't exist attribute "address",
linter give an error message with `exit code 1`.
Attribute "short_name" passed because it's not same with "name".

```
$ hanum sample.json
[ERROR] Default presets currently didn't available!
```

Since currently we are didn't have any default presets, we can't lint it.

## Test it Out
This library written in Haskell with The Haskell Tool Stack.
Below is an installation and demo for the UNIX-like operating system.

```
$ git clone https://github.com/wisn/hanum.git
$ cd hanum
$ stack install
$ hanum version
Hanum 0.0.1-alpha
$ cd sample
$ hanum sample.json sample.presets.json
```

**NOTE**: Currently the linter have a problem with the "presets" directory.
It's must be in the same directory. We can't do `hanum sample.json another/dir/presets.json`.
