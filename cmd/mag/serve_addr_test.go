package main

import "testing"

func TestLanguageServerAddrDefaultsToLoopback(t *testing.T) {
	if got := languageServerAddr("127.0.0.1", 4567); got != "127.0.0.1:4567" {
		t.Errorf("languageServerAddr = %q", got)
	}
	if got := languageServerAddr("::1", 4567); got != "[::1]:4567" {
		t.Errorf("IPv6 languageServerAddr = %q", got)
	}
	for host, want := range map[string]bool{
		"127.0.0.1": true, "localhost": true, "::1": true,
		"0.0.0.0": false, "": false, "192.168.1.5": false, "example.com": false,
	} {
		if got := isLoopbackHost(host); got != want {
			t.Errorf("isLoopbackHost(%q) = %v, want %v", host, got, want)
		}
	}
}
