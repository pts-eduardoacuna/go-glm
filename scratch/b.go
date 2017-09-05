package main

import (
	"encoding/binary"
	"fmt"
	"log"
	"os"
)

type LabelReader struct {
	MagicNumber uint32
	LabelCount  uint32
	OpenedFile  *os.File
	LabelBuffer []byte
}

func NewLabelReader(path string, magic uint32) (*LabelReader, error) {
	// Open the file
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}

	buffer := make([]byte, 4)
	reader := &LabelReader{0, 0, file, nil}

	// Read it's header
	_, err = file.Read(buffer)
	if err != nil {
		file.Close()
		return nil, err
	}
	reader.MagicNumber = binary.BigEndian.Uint32(buffer)

	if reader.MagicNumber != magic {
		file.Close()
		return nil, fmt.Errorf("expecting magic number to be %s, but the parsed number is %s", magic, reader.MagicNumber)
	}

	_, err = file.Read(buffer)
	if err != nil {
		file.Close()
		return nil, err
	}
	reader.LabelCount = binary.BigEndian.Uint32(buffer)

	// Create the label buffer
	reader.LabelBuffer = make([]byte, 1)

	return reader, nil
}

func (r *LabelReader) Read(b []byte) (int, error) {
	count, err := r.OpenedFile.Read(b)
	if err != nil {
		r.OpenedFile.Close()
	}
	return count, err
}

func (r *LabelReader) Close() error {
	return r.OpenedFile.Close()
}

func (r *LabelReader) ReadLabel() (byte, error) {
	_, err := r.Read(r.LabelBuffer)
	return r.LabelBuffer[0], err
}

type ImageReader struct {
	MagicNumber uint32
	ImageCount  uint32
	RowCount    uint32
	ColumnCount uint32
	OpenedFile  *os.File
}

func NewImageReader(path string, magic uint32) (*ImageReader, error) {
	// Open the file
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}

	buffer := make([]byte, 4)
	reader := &ImageReader{0, 0, 0, 0, file}

	// Read it's header
	_, err = file.Read(buffer)
	if err != nil {
		file.Close()
		return nil, err
	}
	reader.MagicNumber = binary.BigEndian.Uint32(buffer)

	if reader.MagicNumber != magic {
		file.Close()
		return nil, fmt.Errorf("expecting magic number to be %s, but the parsed number is %s", magic, reader.MagicNumber)
	}

	_, err = file.Read(buffer)
	if err != nil {
		file.Close()
		return nil, err
	}
	reader.ImageCount = binary.BigEndian.Uint32(buffer)

	_, err = file.Read(buffer)
	if err != nil {
		file.Close()
		return nil, err
	}
	reader.RowCount = binary.BigEndian.Uint32(buffer)

	_, err = file.Read(buffer)
	if err != nil {
		file.Close()
		return nil, err
	}
	reader.ColumnCount = binary.BigEndian.Uint32(buffer)

	return reader, nil
}

func (r *ImageReader) Read(b []byte) (int, error) {
	count, err := r.OpenedFile.Read(b)
	if err != nil {
		r.OpenedFile.Close()
	}
	return count, err
}

func (r *ImageReader) ReadImage() ([]byte, error) {
	b := make([]byte, r.RowCount*r.ColumnCount)
	_, err := r.Read(b)
	return b, err
}

func (r *ImageReader) Close() error {
	return r.OpenedFile.Close()
}

func main() {
	imagesPath := "../mnist/testing-images"
	imageReader, err := NewImageReader(imagesPath, 2051)
	if err != nil {
		log.Fatal("Error creating MNIST image reader", err)
	}
	defer imageReader.Close()

	fmt.Println(imageReader.ImageCount)
	fmt.Println(imageReader.RowCount)
	fmt.Println(imageReader.ColumnCount)

	labelsPath := "../mnist/testing-labels"
	labelReader, err := NewLabelReader(labelsPath, 2049)
	if err != nil {
		log.Fatal("Error creating MNIST label reader", err)
	}
	defer labelReader.Close()

	fmt.Println(labelReader.LabelCount)

	// Plot some images
	for i := uint32(0); i < imageReader.ImageCount/100; i++ {
		img, err := imageReader.ReadImage()
		if err != nil {
			log.Fatal("Error reading MNIST image", err)
		}
		lbl, err := labelReader.ReadLabel()
		if err != nil {
			log.Fatal("Error reading MNIST label", err)
		}

		fmt.Print("\n\n█████████████████████████████████████████████████████████\n")
		fmt.Println("Should be", lbl)
		PlotImage(img, imageReader.RowCount, imageReader.ColumnCount)
	}
}

func PlotImage(img []byte, rows, cols uint32) {
	on, off := "▓▓", "░░"
	for i := uint32(0); i < rows*cols; i++ {
		if i%cols == 0 {
			fmt.Print("\n")
		}
		if img[i] == 0 {
			fmt.Print(off)
		} else {
			fmt.Print(on)
		}
	}
	fmt.Print("\n")
}
