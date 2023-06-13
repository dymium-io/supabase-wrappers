const path = require('path');
const { override, babelInclude } = require('customize-cra');

module.exports = function (config, env) {
  // Add the aliases for the dist directories
  return Object.assign(
    config,
    override(
      babelInclude([
        path.resolve('src'),
        path.resolve('../common'),
        path.resolve('../common/Api'),
        path.resolve('../contrib/react-bootstrap-table-next'),
        path.resolve('../contrib/react-bootstrap-table2-toolkit'),
        path.resolve('../contrib/react-bootstrap-table2-paginator'),
      ]),
    )(config, env),
  );
};