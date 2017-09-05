package main

import (
	"encoding/csv"
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/pts-eduardoacuna/go-glm/mnist"
)

func main() {
	var inputDir string
	var outputDir string

	flag.StringVar(&inputDir, "inputs", "", "The path of the directory where inputs are located")
	flag.StringVar(&outputDir, "outputs", "", "The path of the directory where outputs are located")

	flag.Parse()

	if inputDir == "" {
		log.Fatal("no input directory given")
	}

	if outputDir == "" {
		log.Fatal("no output directory given")
	}

	if stat, err := os.Stat(inputDir); err != nil || !stat.IsDir() {
		log.Fatalf("input argument is not a valid directory (%s)", inputDir)
	}

	if stat, err := os.Stat(outputDir); err != nil || !stat.IsDir() {
		log.Fatalf("output argument is not a valid directory (%s)", outputDir)
	}

	trainingImagesPath := inputDir + "/training-images"
	trainingLabelsPath := inputDir + "/training-labels"
	trainingCSVPath := outputDir + "/training.csv"
	predictImagesPath := inputDir + "/predict-images"
	predictLabelsPath := inputDir + "/predict-labels"
	predictCSVPath := outputDir + "/predict.csv"

	Convert(trainingImagesPath, trainingLabelsPath, trainingCSVPath)
	Convert(predictImagesPath, predictLabelsPath, predictCSVPath)
}

func Convert(imagesPath, labelsPath, csvPath string) {
	imagesReader, err := mnist.NewImageReader(imagesPath, 2051)
	if err != nil {
		log.Fatal(err)
	}
	defer imagesReader.Close()

	labelsReader, err := mnist.NewLabelReader(labelsPath, 2049)
	if err != nil {
		log.Fatal(err)
	}
	defer labelsReader.Close()

	outputFile, err := os.Create(csvPath)
	if err != nil {
		log.Fatal(err)
	}
	defer outputFile.Close()

	writer := csv.NewWriter(outputFile)

	for i := uint32(0); i < imagesReader.ImageCount; i++ {
		img, err := imagesReader.ReadImage()
		if err != nil {
			log.Fatal(err)
		}
		lbl, err := labelsReader.ReadLabel()
		if err != nil {
			log.Fatal(err)
		}

		record := Clean(img, lbl)
		writer.Write(record)
		if err := writer.Error(); err != nil {
			log.Fatal(err)
		}
	}
}

func Clean(img []byte, lbl byte) []string {
	record := make([]string, len(img)+1)
	record[0] = fmt.Sprintf("%d", lbl)

	for i, b := range img {
		if b == 0 {
			record[i+1] = "0"
		} else {
			record[i+1] = "1"
		}
	}
	return record
}
