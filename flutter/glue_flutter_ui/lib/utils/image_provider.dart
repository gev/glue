import 'dart:io';

import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

Eval<Ir> Function(Ir props) imageProviderImpl(
  ImageProvider Function(String src) makeImage,
) => (Ir props) {
  switch (props) {
    case IrObject(:final properties):
      final src = properties['src'];
      switch (src) {
        case IrString(:final value):
        case IrSymbol(:final value):
        case IrDottedSymbol(:final value):
          return Eval.pure(IrNativeValue(Value(makeImage(value))));
        default:
          return throwError(
            wrongArgumentType([
              '`src` should be a `String`, `Symbol` or `DottedSymol`',
            ]),
          );
      }
    default:
      return throwError(wrongArgumentType(['object']));
  }
};

ImageProvider assetImage(String src) => AssetImage(src);
ImageProvider fileImage(String src) => FileImage(File(src));
ImageProvider networkImage(String src) => NetworkImage(src);
