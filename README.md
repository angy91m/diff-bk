# diff-bk

## Install

```bash
    npm i diff-bk
```

## Usage

```javascript
const diffBk = require('diff-bk');
const s0 = `\n
\n
Lorem 'added' ipsum dolor sit amet, consectetur adipiscing elit.
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
Fusce dapibus, tellus ac cursus commodo, tortor mauris condimentum nibh, ut fermentum massa justo sit amet risus.
Mauris in erat justo. Nullam ac urna eu felis dapibus condimentum sit amet a augue.
Curabitur blandit tempus porttitor.
Sed non neque elit. Sed ut imperdiet nisi. Proin condimentum fermentum nunc.
Etiam pharetra, erat sed fermentum feugiat, velit mauris egestas quam, ut aliquam massa nisl quis neque.
Suspendisse in orci enim.
Integer in mi justo, a sollicitudin orci.
Duis sed odio sit amet nibh vulputate cursus a sit amet mauris.
Morbi accumsan ipsum velit. Nam nec tellus a odio tincidunt auctor a ornare odio.
Sed non mauris vitae erat consequat auctor eu in elit.
Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos.
Mauris in erat justo. Nullam ac urna eu felis dapibus condimentum sit amet a augue.
Sed non neque elit. Sed ut imperdiet nisi. Proin condimentum fermentum nunc.
Etiam pharetra, erat sed fermentum feugiat, velit mauris egestas quam, ut aliquam massa nisl quis neque.
Suspendisse in orci enim.
Integer in mi justo, a sollicitudin orci.
Duis sed odio sit amet nibh vulputate cursus a sit amet mauris.
Morbi accumsan ipsum velit. Nam nec tellus a odio tincidunt auctor a ornare odio.
Sed non mauris vitae erat consequat auctor eu in elit.
Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos.\t`;

const s1 = `Lorem ipsum 'removed' dolor sit amet, consectetur adipiscing elit.
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
Suspendisse in orci enim.
Antanicam erat volutpat.
AnBJGK ijono rat volutpat.
Integer in mi justo, a sollicitudin orci.
Duis sed odio sit amet nibh vulputate cursus a sit amet mauris.
Morbi accumsan ipsum velit. Nam nec tellus a odio tincidunt auctor a ornare odio.
Sed non mauris vitae erat consequat auctor eu in elit.
Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos.
Mauris in erat justo. Nullam ac urna eu felis dapibus condimentum sit amet a augue.
Sed non neque elit. Sed ut imperdiet nisi. Proin condimentum fermentum nunc.
Curabitur blandit tempus porttitor.
Mauris in erat justo. Nullam ac urna eu felis dapibus condimentum sit amet a augue.
Sed non neque elit. Sed ut imperdiet nisi. Proin condimentum fermentum nunc.
Etiam pharetra, erat sed fermentum feugiat, velit mauris egestas quam, ut aliquam massa nisl quis neque.
Suspendisse in orci enim.
Integer in mi justo, a sollicitudin orci.
Duis sed odio sit amet nibh vulputate cursus a sit amet mauris.
Morbi accumsan ipsum velit. Nam nec tellus a odio tincidunt auctor a ornare odio.
Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos.`;

const diffs = diffBk.diff(s0, s1);
console.log(diffs);

/*

[
  { '-': '\n\n\n' },
  { m: [ -6, "'added' ", -5, 10, -45 ] },
  { '=': 1 },
  { '~': 7 },
  { '=': 2 },
  {
    '-': 'Fusce dapibus, tellus ac cursus commodo, tortor mauris condimentum nibh, ut fermentum massa justo sit amet risus.\n' +
      'Etiam pharetra, erat sed fermentum feugiat, velit mauris egestas quam, ut aliquam massa nisl quis neque.'
  },
  { '=': 1 },
  { '+': 2 },
  { '=': 7 },
  { '~': [ 11, 10, 12 ] },
  { '=': 5 },
  { '-': 'Sed non mauris vitae erat consequat auctor eu in elit.' },
  { m: [ -91, '\t' ] }
]

*/


const s2 = diffBk.restore(s1,diffs);
console.log(s0 == s2);

//true
```