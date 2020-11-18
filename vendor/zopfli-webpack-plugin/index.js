'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _url = require('url');

var _url2 = _interopRequireDefault(_url);

var _async = require('async');

var _async2 = _interopRequireDefault(_async);

var _zopfli = require('@gfx/zopfli');

var _RawSource = require('webpack-sources/lib/RawSource');

var _RawSource2 = _interopRequireDefault(_RawSource);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var ZopfliPlugin = function () {
  function ZopfliPlugin() {
    var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

    _classCallCheck(this, ZopfliPlugin);

    this.asset = options.asset || '[path].gz[query]';
    this.algorithm = options.algorithm || 'gzip';
    this.filename = options.filename || false;
    this.compressionOptions = {};

    if (typeof this.algorithm === 'string') {
      this.compressionOptions = {
        verbose: hasOwnProperty.call(options, 'verbose') ? options.verbose : false,
        verbose_more: hasOwnProperty.call(options, 'verbose_more') ? options.verbose_more : false,
        numiterations: options.numiterations ? options.numiterations : 15,
        blocksplitting: hasOwnProperty.call(options, 'blocksplitting') ? options.blocksplitting : true,
        blocksplittingmax: options.blocksplittingmax ? options.blocksplittingmax : 15
      };
      this.algorithm = function (content, opts, fn) {
        (0, _zopfli.gzip)(content, options, fn);
      };
    } else if (!this.algorithm) {
      throw new Error('Algorithm incorrect or not found');
    }
    this.test = options.test || options.regExp;
    this.threshold = options.threshold || 0;
    this.minRatio = options.minRatio || 0.8;
    this.deleteOriginalAssets = options.deleteOriginalAssets || false;
  }

  _createClass(ZopfliPlugin, [{
    key: 'apply',
    value: function apply(compiler) {
      var _this = this;

      var processAssets = function processAssets(compilation, callback) {
        var assets = compilation.assets;


        return _async2.default.forEach(Object.keys(assets), function (file, cb) {
          if (Array.isArray(_this.test)) {
            if (_this.test.every(function (t) {
              return !t.test(file);
            })) {
              return cb();
            }
          } else if (_this.test && !_this.test.test(file)) {
            return cb();
          }

          var asset = assets[file];
          var content = asset.source();

          if (!Buffer.isBuffer(content)) {
            content = new Buffer(content, 'utf-8');
          }

          var originalSize = content.length;

          if (originalSize < _this.threshold) {
            return cb();
          }

          return _this.algorithm(content, _this.compressionOptions, function (err, result) {
            if (err) {
              return cb(err);
            }

            if (result.length / originalSize > _this.minRatio) {
              return cb();
            }

            var parse = _url2.default.parse(file);
            var sub = {
              file,
              path: parse.pathname,
              query: parse.query || ''
            };

            var newAsset = _this.asset.replace(/\[(file|path|query)\]/g, function (p0, p1) {
              return sub[p1];
            });

            if (typeof _this.filename === 'function') {
              newAsset = _this.filename(newAsset);
            }

            assets[newAsset] = new _RawSource2.default(result);

            if (_this.deleteOriginalAssets) {
              delete assets[file];
            }

            return cb();
          });
        }, callback);
      };

      if (compiler.hooks) {
        compiler.hooks.emit.tapAsync('ZopfliPlugin', processAssets);
      } else {
        compiler.plugin('emit', processAssets);
      }
    }
  }]);

  return ZopfliPlugin;
}();

exports.default = ZopfliPlugin;