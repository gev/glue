class Context {
  final Map<Type, dynamic> _pool;

  Context.empty() : _pool = {};

  Context(this._pool);

  @override
  bool operator ==(Object other) =>
      identical(this, other) || (other is Context && (other._pool == _pool));

  @override
  int get hashCode => _pool.hashCode;
}

T getFromContext<T>(Context context) => context._pool[T] as T;

Context putToContext<T>(Context context, T obj) =>
    Context({...context._pool, T: obj});
