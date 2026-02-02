// Maggie Documentation Playground
// Sends example code to /api/eval and displays results inline
(function() {
    'use strict';

    document.addEventListener('DOMContentLoaded', function() {
        // Find all example blocks and add Run buttons
        var examples = document.querySelectorAll('.doc-example');
        examples.forEach(function(block) {
            var pre = block.querySelector('pre');
            var code = block.querySelector('code');
            if (!code) return;

            // Create Run button
            var btn = document.createElement('button');
            btn.className = 'playground-run';
            btn.textContent = 'Run';
            btn.addEventListener('click', function() {
                runExample(btn, code.textContent);
            });

            // Create output area
            var output = document.createElement('div');
            output.className = 'playground-output';
            output.style.display = 'none';

            // Insert button before pre, output after pre
            block.insertBefore(btn, pre);
            block.appendChild(output);
        });
    });

    function runExample(btn, code) {
        var output = btn.parentElement.querySelector('.playground-output');
        btn.disabled = true;
        btn.textContent = 'Running...';
        output.style.display = 'block';
        output.className = 'playground-output playground-loading';
        output.textContent = 'Evaluating...';

        fetch('/api/eval', {
            method: 'POST',
            headers: { 'Content-Type': 'text/plain' },
            body: code
        })
        .then(function(resp) {
            return resp.text().then(function(text) {
                return { ok: resp.ok, status: resp.status, text: text };
            });
        })
        .then(function(result) {
            btn.disabled = false;
            btn.textContent = 'Run';
            output.className = 'playground-output ' + (result.ok ? 'playground-success' : 'playground-error');
            output.textContent = result.text;
        })
        .catch(function(err) {
            btn.disabled = false;
            btn.textContent = 'Run';
            output.className = 'playground-output playground-error';
            output.textContent = 'Error: ' + err.message;
        });
    }
})();
