class Ref<T> {
  T value;
  Ref(this.value);

  @override
  String toString() => 'Ref($value)';

  @override
  bool operator ==(Object other) =>
      identical(this, other) || (other is Ref && value == other.value);

  @override
  int get hashCode => value.hashCode;
}
