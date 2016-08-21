package main

import (
	"bufio"
	"encoding/csv"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"path"
	"strings"
	"sync"
	"syscall"
	"time"
)

var (
	addr   = flag.String("addr", ":8000", "listen addr")
	logdir = flag.String("logdir", "log", "log directory")

	writers = map[string]*csv.Writer{}
	mu      sync.Mutex
)

// LOCKED
func getWriter(p string) (*csv.Writer, error) {
	if !strings.HasPrefix(p, "/sensor/") {
		return nil, fmt.Errorf("%q doesn't have the correct prefix", p)
	}
	fn := p[len("/sensor/"):]
	cw, ok := writers[fn]
	if ok {
		return cw, nil
	}
	w, err := os.OpenFile(path.Join(*logdir, fn), os.O_APPEND|syscall.O_CREAT, 0666)
	if err != nil {
		return nil, err
	}
	cw = csv.NewWriter(w)
	writers[fn] = cw
	return cw, nil
}

func handler(w http.ResponseWriter, r *http.Request) {
	mu.Lock()
	defer mu.Unlock()
	csvw, err := getWriter(r.URL.Path)
	if err != nil {
		http.Error(w, err.Error(), 500)
		log.Printf("Error getting CSV writer: %v", err)
		return
	}
	sc := bufio.NewScanner(r.Body)
	for sc.Scan() {
		log.Printf("Read %q", sc.Text())
		if err := csvw.Write([]string{time.Now().Format(time.RFC3339Nano), sc.Text()}); err != nil {
			http.Error(w, err.Error(), 500)
			log.Printf("Error writing CSV row: %v", err)
			return
		}
	}
	csvw.Flush()
}

func main() {
	http.HandleFunc("/", handler)
	http.ListenAndServe(*addr, nil)
}
