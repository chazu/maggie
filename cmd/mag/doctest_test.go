package main

import "testing"

// ---------------------------------------------------------------------------
// parseDoctestAssertions
// ---------------------------------------------------------------------------

func TestDoctestParseDoctestAssertions(t *testing.T) {
	tests := []struct {
		name     string
		content  string
		expected []doctestAssertion
	}{
		{
			name:     "empty content",
			content:  "",
			expected: nil,
		},
		{
			name:    "single assertion with expected value",
			content: "3 + 4 >>> 7",
			expected: []doctestAssertion{
				{Line: "3 + 4 >>> 7", Expr: "3 + 4", Expected: "7"},
			},
		},
		{
			name:    "multiple assertions",
			content: "3 + 4 >>> 7\n10 - 2 >>> 8",
			expected: []doctestAssertion{
				{Line: "3 + 4 >>> 7", Expr: "3 + 4", Expected: "7"},
				{Line: "10 - 2 >>> 8", Expr: "10 - 2", Expected: "8"},
			},
		},
		{
			name:    "setup line without expected value",
			content: "x := 42",
			expected: []doctestAssertion{
				{Line: "x := 42", Expr: "x := 42", Expected: ""},
			},
		},
		{
			name:    "setup line followed by assertion",
			content: "x := 42\nx + 1 >>> 43",
			expected: []doctestAssertion{
				{Line: "x := 42", Expr: "x := 42", Expected: ""},
				{Line: "x + 1 >>> 43", Expr: "x + 1", Expected: "43"},
			},
		},
		{
			name:    "blank lines between assertions are skipped",
			content: "3 + 4 >>> 7\n\n\n10 - 2 >>> 8",
			expected: []doctestAssertion{
				{Line: "3 + 4 >>> 7", Expr: "3 + 4", Expected: "7"},
				{Line: "10 - 2 >>> 8", Expr: "10 - 2", Expected: "8"},
			},
		},
		{
			name:    "whitespace-only lines are skipped",
			content: "   \n3 + 4 >>> 7\n\t\n",
			expected: []doctestAssertion{
				{Line: "3 + 4 >>> 7", Expr: "3 + 4", Expected: "7"},
			},
		},
		{
			name:    "leading and trailing whitespace trimmed",
			content: "   3 + 4 >>> 7   ",
			expected: []doctestAssertion{
				{Line: "3 + 4 >>> 7", Expr: "3 + 4", Expected: "7"},
			},
		},
		{
			name:    "expression with string containing >>>",
			// The parser splits on first >>>, so 'hello >>> world' >>> 'result'
			// would split at the first >>>
			content: "x >>> 'expected'",
			expected: []doctestAssertion{
				{Line: "x >>> 'expected'", Expr: "x", Expected: "'expected'"},
			},
		},
		{
			name:     "malformed: nothing before >>> is skipped",
			content:  ">>> 42",
			expected: nil, // empty expr is skipped
		},
		{
			name:     "malformed: nothing after >>> is skipped",
			content:  "42 >>>",
			expected: nil, // empty expected is skipped
		},
		{
			name:    "mixed setup and assertions",
			content: "list := ArrayList new\nlist add: 1\nlist add: 2\nlist size >>> 2\nlist first >>> 1",
			expected: []doctestAssertion{
				{Line: "list := ArrayList new", Expr: "list := ArrayList new", Expected: ""},
				{Line: "list add: 1", Expr: "list add: 1", Expected: ""},
				{Line: "list add: 2", Expr: "list add: 2", Expected: ""},
				{Line: "list size >>> 2", Expr: "list size", Expected: "2"},
				{Line: "list first >>> 1", Expr: "list first", Expected: "1"},
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := parseDoctestAssertions(tc.content)

			if len(got) != len(tc.expected) {
				t.Fatalf("len mismatch: got %d, want %d\ngot:  %+v\nwant: %+v",
					len(got), len(tc.expected), got, tc.expected)
			}

			for i := range got {
				if got[i].Line != tc.expected[i].Line {
					t.Errorf("[%d] Line: got %q, want %q", i, got[i].Line, tc.expected[i].Line)
				}
				if got[i].Expr != tc.expected[i].Expr {
					t.Errorf("[%d] Expr: got %q, want %q", i, got[i].Expr, tc.expected[i].Expr)
				}
				if got[i].Expected != tc.expected[i].Expected {
					t.Errorf("[%d] Expected: got %q, want %q", i, got[i].Expected, tc.expected[i].Expected)
				}
			}
		})
	}
}

// ---------------------------------------------------------------------------
// tallyDoctestResults
// ---------------------------------------------------------------------------

func TestDoctestTallyDoctestResults(t *testing.T) {
	tests := []struct {
		name       string
		results    []doctestMethodResult
		wantPass   int
		wantFail   int
		wantTotal  int
	}{
		{
			name:      "empty results",
			results:   nil,
			wantPass:  0,
			wantFail:  0,
			wantTotal: 0,
		},
		{
			name: "all passing assertions",
			results: []doctestMethodResult{
				{
					ClassName: "Array",
					Selector:  "size",
					Results: []doctestResult{
						{Assertion: doctestAssertion{Expected: "3"}, Passed: true},
						{Assertion: doctestAssertion{Expected: "5"}, Passed: true},
					},
				},
			},
			wantPass:  2,
			wantFail:  0,
			wantTotal: 2,
		},
		{
			name: "all failing assertions",
			results: []doctestMethodResult{
				{
					ClassName: "Array",
					Selector:  "size",
					Results: []doctestResult{
						{Assertion: doctestAssertion{Expected: "3"}, Passed: false},
						{Assertion: doctestAssertion{Expected: "5"}, Passed: false},
					},
				},
			},
			wantPass:  0,
			wantFail:  2,
			wantTotal: 2,
		},
		{
			name: "mixed pass and fail",
			results: []doctestMethodResult{
				{
					ClassName: "Array",
					Selector:  "at:",
					Results: []doctestResult{
						{Assertion: doctestAssertion{Expected: "1"}, Passed: true},
						{Assertion: doctestAssertion{Expected: "2"}, Passed: false},
						{Assertion: doctestAssertion{Expected: "3"}, Passed: true},
					},
				},
			},
			wantPass:  2,
			wantFail:  1,
			wantTotal: 3,
		},
		{
			name: "successful setup lines are not counted",
			results: []doctestMethodResult{
				{
					ClassName: "Array",
					Selector:  "collect:",
					Results: []doctestResult{
						// Setup line: Expected is empty, Passed is true => not counted
						{Assertion: doctestAssertion{Expected: ""}, Passed: true},
						{Assertion: doctestAssertion{Expected: "42"}, Passed: true},
					},
				},
			},
			wantPass:  1,
			wantFail:  0,
			wantTotal: 1,
		},
		{
			name: "failed setup lines are counted as failures",
			results: []doctestMethodResult{
				{
					ClassName: "Array",
					Selector:  "do:",
					Results: []doctestResult{
						// Failed setup: Expected is empty, Passed is false => counted
						{Assertion: doctestAssertion{Expected: ""}, Passed: false},
						{Assertion: doctestAssertion{Expected: "10"}, Passed: true},
					},
				},
			},
			wantPass:  1,
			wantFail:  1,
			wantTotal: 2,
		},
		{
			name: "multiple method results across classes",
			results: []doctestMethodResult{
				{
					ClassName: "Array",
					Selector:  "size",
					Results: []doctestResult{
						{Assertion: doctestAssertion{Expected: "3"}, Passed: true},
					},
				},
				{
					ClassName: "String",
					Selector:  "size",
					Results: []doctestResult{
						{Assertion: doctestAssertion{Expected: "5"}, Passed: false},
						{Assertion: doctestAssertion{Expected: "0"}, Passed: true},
					},
				},
			},
			wantPass:  2,
			wantFail:  1,
			wantTotal: 3,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			gotPass, gotFail, gotTotal := tallyDoctestResults(tc.results)

			if gotPass != tc.wantPass {
				t.Errorf("passed: got %d, want %d", gotPass, tc.wantPass)
			}
			if gotFail != tc.wantFail {
				t.Errorf("failed: got %d, want %d", gotFail, tc.wantFail)
			}
			if gotTotal != tc.wantTotal {
				t.Errorf("total: got %d, want %d", gotTotal, tc.wantTotal)
			}
		})
	}
}
